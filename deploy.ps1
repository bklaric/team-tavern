# Deploy a server: build it, upload it as a new release, bring the stack up from that release, and
# check that it answers. Run from the repo root.
#
#   .\deploy.ps1 -server user@host
#   .\deploy.ps1 -server user@host -environment staging
#   .\deploy.ps1 -server user@host -schemaApplied
#
# The server holds no checkout. Everything lives under ~/team-tavern there:
#   .env               the server's own configuration, with the real keys and the ENVIRONMENT it
#                      runs. Every release links to it, and the nightly backup reads it.
#   prod/              Postgres data and backups and Caddy's certificates, which every release mounts.
#   releases/<id>/     one unpacked dist/<environment> per deploy; the newest five are kept.
#   releases/current   a link to the release that is up. Its REVISION names the commit it was built
#                      from on the first line, then the commit of each local spago dependency.
#
# Migrations are applied by hand. When TablesCurrent.sql has changed since the release that is up,
# apply the migration on the server, then deploy with -schemaApplied. The same switch goes ahead
# when the revision that is up can't be compared, for example when it isn't in this checkout.
#
# Roll back by bringing up an earlier release:
#   ssh user@host "cd team-tavern/releases && docker compose -f <id>/docker-compose.yml up -d --remove-orphans --force-recreate && ln -sfn <id> current"
#
# Remote commands are single lines with only single quotes inside: Windows PowerShell mangles
# double quotes nested in a native command's arguments, and a script piped to ssh would arrive
# with CRLF line endings.
param (
    [string] $server,

    [ValidateSet("staging", "production")]
    [string] $environment = "production",

    [switch] $schemaApplied
)

# Stop at the first failing cmdlet. Set per cmdlet rather than through $ErrorActionPreference,
# which in Windows PowerShell also turns ssh's and scp's stderr output into terminating errors
# when the output is redirected.
$PSDefaultParameterValues = @{ "*:ErrorAction" = "Stop" }

$usage = "Usage: .\deploy.ps1 -server <user@host> [-environment <staging|production>] [-schemaApplied]"
$environment = $environment.ToLower()
$site = @{ staging = "https://staging.teamtavern.net"; production = "https://www.teamtavern.net" }[$environment]
$distDir = "dist/$environment"
$tarball = "dist/release.tar.gz"
$schema = "src/TeamTavern/Database/TablesCurrent.sql"
# How long the node container gets to install its packages and open its port before the site is
# given up on.
$answerSeconds = 120
# How long the stack runs before the containers are checked, so a container that fails soon
# after it starts has done so.
$settleSeconds = 15
# How many releases stay on the server, the one that is up included.
$keptReleases = 5

function Fail([string] $message) {
    Write-Output $message
    exit 1
}

# Native tools don't stop the script when they fail, so each one is checked after it runs.
function Assert-Success([string] $what) {
    if ($LASTEXITCODE -ne 0) {
        Fail "$what failed."
    }
}

if ([string]::IsNullOrWhiteSpace($server)) {
    Write-Output "No server provided."
    Fail $usage
}

Write-Output "Deploying $environment to $server."



### Local checks ###

# The release is stamped with the commits it was built from, so each has to be exactly what gets
# built: this repo, and every local checkout spago.yaml compiles in as a path dependency.
$dependencies = @(Select-String -Path spago.yaml -Pattern '^\s+path:\s*(\S+)' | ForEach-Object { $_.Matches[0].Groups[1].Value })
$dirtyRepos = @()
foreach ($repo in @(".") + $dependencies) {
    $dirty = git -C $repo status --porcelain
    Assert-Success "git status in $repo"
    if ($dirty) {
        $dirtyRepos += $repo
    }
}
if ($dirtyRepos.Count -gt 0) {
    Fail ("Uncommitted changes in " + ($dirtyRepos -join ", ") + ". Commit or stash them before deploying.")
}

$revision = git rev-parse HEAD
Assert-Success "git rev-parse"
$id = "{0}-{1}" -f (Get-Date -Format "yyyyMMdd-HHmmss"), $revision.Substring(0, 7)

$revisionLines = @($revision)
foreach ($dependency in $dependencies) {
    $dependencyRevision = git -C $dependency rev-parse HEAD
    Assert-Success "git rev-parse in $dependency"
    $revisionLines += "$dependency $dependencyRevision"
}

# Compose refuses to start without a variable the stack requires, ${NAME:?}, and by then the
# release is unpacked and the stack half switched, so the server's .env is checked for each first.
$required = @(Select-String -Path stacks/server.docker-compose.yml -Pattern '\$\{([A-Z_]+):\?\}' -AllMatches |
    ForEach-Object { $_.Matches } | ForEach-Object { $_.Groups[1].Value } | Sort-Object -Unique)



### Server checks ###

# Without its data directory Postgres would initialise a fresh, empty database rather than fail,
# so a missing one stops the deploy here, as does a .env that lacks a required variable or names
# another environment than the build's. Containers named tt-* that another compose project owns
# would block `up` on their names and ports, so those stop it too. Prints the release that is up
# and its revision, if any.
$preflight = 'mkdir -p team-tavern/releases && cd team-tavern' +
    ' && { test -d prod/docker-postgres/postgresql || { echo ''No Postgres data in ~/team-tavern/prod/docker-postgres/postgresql.'' >&2; exit 1; }; }' +
    ' && { test -f .env || { echo ''No ~/team-tavern/.env.'' >&2; exit 1; }; }' +
    ' && for name in ' + ($required -join ' ') + '; do grep -q ^$name=. .env || { echo ~/team-tavern/.env sets no $name. >&2; exit 1; }; done' +
    " && { grep -qx ENVIRONMENT=$environment .env || { echo '~/team-tavern/.env sets ENVIRONMENT to something other than $environment.' >&2; exit 1; }; }" +
    ' && docker compose version >/dev/null' +
    ' && { test $(docker ps -aq --filter name=tt- | wc -l) -eq $(docker ps -aq --filter name=tt- --filter label=com.docker.compose.project=teamtavern | wc -l) || { echo ''Containers named tt-* belong to a compose project other than teamtavern. Bring that stack down first; docker compose ls lists it.'' >&2; exit 1; }; }' +
    ' && if [ -e releases/current ]; then readlink releases/current && head -n 1 releases/current/REVISION; fi'
$current = @(ssh $server $preflight)
Assert-Success "Checking the server"

$previousRelease = $null
$previousRevision = $null
if ($current.Count -ge 2) {
    $previousRelease = $current[0].Trim()
    $previousRevision = $current[1].Trim()
    Write-Output "Up now: $previousRelease ($previousRevision)."
}
else {
    Write-Output "No release is up yet."
}

# Migrations are applied by hand, so a deploy that changes the schema waits until that is done.
if ($previousRevision) {
    git diff --quiet $previousRevision HEAD -- $schema
    if ($LASTEXITCODE -eq 1) {
        if (-not $schemaApplied) {
            Fail "$schema has changed since $previousRevision. Apply the migration on the server, then deploy with -schemaApplied."
        }
        Write-Output "$schema has changed; going ahead because -schemaApplied says the migration is in place."
    }
    elseif ($LASTEXITCODE -ne 0) {
        if (-not $schemaApplied) {
            Fail "Couldn't compare $schema with $previousRevision, the revision that is up. Once the schema on the server matches this one, deploy with -schemaApplied."
        }
        Write-Output "Couldn't compare $schema with $previousRevision; going ahead because -schemaApplied says it's in place."
    }
}
else {
    Write-Output "Nothing to compare $schema against; skipping the schema check."
}



### Build ###

& .\bundle.ps1 -environment $environment
Assert-Success "The build"

# The release is exactly this list: nothing missing, and nothing a local build would add, the .env
# above all, which would shadow the server's own.
$expected = @(
    "docker-compose.yml", "Caddyfile", "base.Caddyfile", "backup-database.sh",
    "client/index.html", "client/index.prerender.html",
    "server/server.js", "server/package.json", "server/package-lock.json"
)
$missing = @($expected | Where-Object { -not (Test-Path "$distDir/$_") })
if ($missing.Count -gt 0) {
    Fail ("The build is missing " + ($missing -join ", ") + ".")
}
$topLevel = @($expected | ForEach-Object { $_.Split("/")[0] })
$unexpected = @(Get-ChildItem -Force $distDir | Where-Object { $topLevel -notcontains $_.Name } | ForEach-Object { $_.Name })
if ($unexpected.Count -gt 0) {
    Fail ("The build has unexpected entries: " + ($unexpected -join ", ") + ".")
}

# WriteAllText rather than Set-Content, which writes a byte order mark on Windows PowerShell.
[IO.File]::WriteAllText((Join-Path (Get-Location) "$distDir/REVISION"), (($revisionLines -join "`n") + "`n"))



### Upload ###

# A file rather than a pipe into ssh: Windows PowerShell re-encodes what it pipes between native
# commands, which corrupts binary data.
Remove-Item -Force -ErrorAction SilentlyContinue $tarball
tar -czf $tarball -C $distDir .
Assert-Success "Packing the release"

scp $tarball "${server}:team-tavern/releases/$id.tar.gz"
Assert-Success "Uploading the release"



### Activate ###

# From here on a failure can leave the stack half on the new release, so every way out says how
# to get back.
function Fail-WithRollback([string] $message) {
    Write-Output $message
    if ($previousRelease) {
        Write-Output "To roll back to ${previousRelease}:"
        Write-Output "  ssh $server ""cd team-tavern/releases && docker compose -f $previousRelease/docker-compose.yml up -d --remove-orphans --force-recreate && ln -sfn $previousRelease current"""
    }
    exit 1
}

# A page that loaded the old index just before the switch still asks for the old build's script
# and stylesheet, so the ones the release that is up names are copied into the new one. Every
# build names its own, so they never collide, and only the previous build's come across.
$keepPreviousAssets = 'if [ -e current ]; then for asset in $(grep -o -e ''app\.min\.[0-9a-f]*\.js'' -e ''style\.min\.[0-9a-f]*\.css'' current/client/index.html); do cp current/client/$asset ' + $id + '/client/ || exit 1; done; fi'

# A fresh directory per release, so files dropped from the build are gone from the server too.
# The release links the server's .env, since compose reads it from the release's own directory.
# The files are bind-mounted, so the containers are recreated to pick them up. Compose is pointed
# at the release itself, not at `current`: it resolves the ../../prod mounts against the path it
# is given.
$activate = "cd team-tavern/releases && mkdir $id && tar -xzf $id.tar.gz -C $id && rm $id.tar.gz && ln -s ../../.env $id/.env && $keepPreviousAssets && docker compose -f $id/docker-compose.yml up -d --remove-orphans --force-recreate && ln -sfn $id current"
ssh $server $activate
if ($LASTEXITCODE -ne 0) {
    Fail-WithRollback "Bringing up $id failed."
}



### Health check ###

# Through Caddy, a node container that is down or still installing its packages answers 502, and
# the game list needs the database too.
$probes = @(
    @{ path = "/"; expected = "200" },
    @{ path = "/api/games"; expected = "200" }
)
$deadline = (Get-Date).AddSeconds($answerSeconds)
do {
    foreach ($probe in $probes) {
        $probe.status = curl.exe -s -o NUL -w "%{http_code}" "$site$($probe.path)"
    }
    $failing = @($probes | Where-Object { $_.status -ne $_.expected })
    if ($failing.Count -eq 0) {
        break
    }
    Start-Sleep -Seconds 2
} while ((Get-Date) -lt $deadline)

if ($failing.Count -gt 0) {
    $answers = $failing | ForEach-Object { "GET $site$($_.path) answered $($_.status), not $($_.expected)." }
    Fail-WithRollback ($answers -join "`n")
}

# A container that crashes after it answered shows up only as one that has stopped or restarted.
# Every container was just recreated with a restart count of zero, and the stack gets a while to
# run first.
Start-Sleep -Seconds $settleSeconds
$containers = @(ssh $server 'docker inspect --format ''{{.Name}} {{.State.Status}} {{.RestartCount}}'' $(docker compose -p teamtavern ps -aq)')
if ($LASTEXITCODE -ne 0) {
    Fail-WithRollback "Listing the containers failed."
}
$unhealthy = @($containers | Where-Object { $_ -notmatch ' running 0$' } | ForEach-Object { $_.TrimStart("/") })
if ($unhealthy.Count -gt 0) {
    Fail-WithRollback ("Not running cleanly (name, state, restarts): " + ($unhealthy -join "; ") + ".")
}

Write-Output "Deployed $id ($revision) to $site."

# Pruned only now, so a failed deploy never deletes the release it says to roll back to; that
# release is kept even when it is older than the newest few.
$prune = "cd team-tavern/releases && ls -1d 2*/ | tr -d / | sort | head -n -$keptReleases | grep -vx -e $id -e '$previousRelease' | xargs -r rm -rf"
ssh $server $prune
if ($LASTEXITCODE -ne 0) {
    Write-Warning "Pruning old releases failed; they stay in ~/team-tavern/releases until the next deploy."
}
