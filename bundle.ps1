# Build TeamTavern for one environment into dist/<environment>/. Run from the repo root.
#
#   .\bundle.ps1 -environment local
#   .\bundle.ps1 -environment local -target client
#   .\bundle.ps1 -environment production
#
# Environments:
#   local      - the stack the tests and local development share, with stand-ins for Discord,
#                SES, Steam and Google. up.ps1 and the Playwright stack setup run it.
#   staging    - what deploy.ps1 ships to a staging server.
#   production - what deploy.ps1 ships to the production server.
#
# Targets (both by default):
#   client - the browser app, into dist/<environment>/client.
#   server - everything else: the API server, the stack's compose file and Caddyfiles, and for
#            local the stand-ins and the .env, for a server the database backup script.
#
# Each environment builds into its own directory, so a production build never replaces the local
# build that the tests run.
param (
    [ValidateSet("local", "staging", "production")]
    [string] $environment,

    [string[]] $target = @("client", "server")
)

# Stop at the first failing cmdlet, so a missing file can't leave a build that looks complete.
# Set per cmdlet rather than through $ErrorActionPreference, which in Windows PowerShell also turns
# a native tool's progress output on a redirected stderr into a terminating error.
$PSDefaultParameterValues = @{ "*:ErrorAction" = "Stop" }

# Native tools don't stop the script when they fail, so each one is checked after it runs.
function Assert-Success([string] $what) {
    if ($LASTEXITCODE -ne 0) {
        Write-Output "$what failed."
        exit 1
    }
}

$validTargets = @("client", "server")
$usage = "Usage: .\bundle.ps1 -environment <local|staging|production> [-target <client,server>]"

# The environment is required: there is no sane default to fall back on.
if ([string]::IsNullOrWhiteSpace($environment)) {
    Write-Output "No environment provided."
    Write-Output $usage
    exit 1
}

$environment = $environment.ToLower()

# Accepts both -target client,server (a PowerShell array) and -target "client,server" (a single string).
$targets = @($target | ForEach-Object { $_ -split "," } | ForEach-Object { $_.Trim().ToLower() } | Where-Object { $_ })

if ($targets.Count -eq 0) {
    $targets = $validTargets
}

$unknownTargets = @($targets | Where-Object { $validTargets -notcontains $_ })

if ($unknownTargets.Count -gt 0) {
    Write-Output ("Unknown target provided: " + ($unknownTargets -join ", "))
    Write-Output "Valid values are ""client"" and ""server""."
    Write-Output $usage
    exit 1
}

$buildClient = $targets -contains "client"
$buildServer = $targets -contains "server"

Write-Output ("Environment: " + $environment)
Write-Output ("Targets: " + ($targets -join ", "))

$distDir = "dist/$environment"
$clientDir = "$distDir/client"

# The tools come from devDependencies, called by path, so every machine builds with the same
# versions and nothing has to be installed globally. node_modules/.bin stays off PATH: spago would
# find the purs.cmd shim there, which Node refuses to spawn.
$bin = "node_modules/.bin"

New-Item -ItemType Directory -Force -Path $distDir | Out-Null

& "$bin/spago.cmd" build
Assert-Success "spago build"

# PureScript compiles each module into an ES module that exports main without running it, so each
# bundle's entry point is a stub on stdin that imports main and calls it.
function Bundle([string] $module, [string] $outfile, [string[]] $flags) {
    "import { main } from './output/TeamTavern.$module/index.js'; main();" |
    & "$bin/esbuild.cmd" --bundle "--outfile=$outfile" @flags
    Assert-Success "Bundling $outfile"
}



### Client ###

if ($buildClient) {

    # Emptied rather than removed: a running Caddy has the directory itself mounted.
    New-Item -ItemType Directory -Force -Path $clientDir | Out-Null
    Get-ChildItem -Force $clientDir | Remove-Item -Force -Recurse

    # Client/Static is copied as it is, file by file, so a new file there needs a line here.
    $static = "src/TeamTavern/Client/Static"
    foreach ($file in @("index.html", "index.prerender.html", "ads.txt", "favicon.svg", "logo-mark.svg", "logo-512.png")) {
        Copy-Item "$static/$file" $clientDir
    }
    New-Item -ItemType Directory -Path "$clientDir/fonts" | Out-Null
    Copy-Item "$static/Fonts/*" "$clientDir/fonts"

    node build-covers.mjs "$static/Images/Games" "$clientDir/images/games"
    Assert-Success "Building the covers"

    # The script and the stylesheet are named anew on every build, so a browser can keep them for
    # good and still never runs an old build after a deploy.
    $discriminator = [Guid]::NewGuid().ToString("N").Substring(0, 16)
    $script = "app.min.$discriminator.js"
    $stylesheet = "style.min.$discriminator.css"

    & "$bin/sass.cmd" src/TeamTavern/Client/Style/Main.scss "$clientDir/$stylesheet" --style compressed
    Assert-Success "Compiling the stylesheet"

    Bundle "Client.Main" "$clientDir/$script" @("--minify")

    # WriteAllText rather than Set-Content, which writes a byte order mark on Windows PowerShell.
    foreach ($index in @("index.html", "index.prerender.html")) {
        $path = Join-Path (Get-Location) "$clientDir/$index"
        $html = [IO.File]::ReadAllText($path)
        if (-not $html.Contains("/app.min.js") -or -not $html.Contains("/style.min.css")) {
            throw "$index doesn't name /app.min.js and /style.min.css, which the build names anew."
        }
        $html = $html.Replace("/app.min.js", "/$script").Replace("/style.min.css", "/$stylesheet")
        [IO.File]::WriteAllText($path, $html, (New-Object Text.UTF8Encoding $false))
    }
}



### Server ###

if ($buildServer) {

    # Remove everything the server target owns, leaving the client in place.
    Get-ChildItem -Force -Path $distDir |
    Where-Object { $_.Name -ne "client" } |
    Remove-Item -Force -Recurse

    # The container installs the three packages the bundle leaves out from these, with --omit=dev,
    # so the build toolchain never enters it.
    New-Item -ItemType Directory -Path "$distDir/server" | Out-Null
    Copy-Item package.json, package-lock.json "$distDir/server"

    # Feed.sql, Fits.sql and the guides are imported as text from where they sit in src/.
    Bundle "Server.Main" "$distDir/server/server.js" @(
        "--platform=node", "--format=cjs",
        "--external:bcrypt", "--external:pg", "--external:@aws-sdk/client-sesv2",
        "--loader:.sql=text", "--loader:.md=text")

    if ($environment -eq "local") {

        # The stand-ins the local stack's server calls in place of Discord, SES, Steam and Google.
        # Only the local build has them, so they never reach a server.
        $stubs = [ordered]@{
            "discord-stub" = "DiscordStub"
            "mail-stub" = "MailStub"
            "steam-stub" = "SteamStub"
            "google-stub" = "GoogleStub"
        }
        foreach ($stub in $stubs.Keys) {
            New-Item -ItemType Directory -Path "$distDir/$stub" | Out-Null
            Bundle "$($stubs[$stub]).Main" "$distDir/$stub/main.js" @("--platform=node", "--format=cjs")
        }

        Copy-Item stacks/local.docker-compose.yml "$distDir/docker-compose.yml"
        Copy-Item stacks/local.env "$distDir/.env"
    }
    else {
        # A server keeps its own .env, which deploy.ps1 links into each release.
        Copy-Item stacks/server.docker-compose.yml "$distDir/docker-compose.yml"
        Copy-Item backup-database.sh $distDir
    }

    Copy-Item stacks/base.Caddyfile $distDir
    Copy-Item "stacks/$environment.Caddyfile" "$distDir/Caddyfile"
}
