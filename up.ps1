# Bring the local stack up from dist/local, which .\bundle.ps1 -environment local builds, at
# http://localhost:8020. Run from the repo root.
#
#   .\up.ps1           builds the site's database from the seed if Postgres doesn't have it yet
#   .\up.ps1 -reseed   builds it anew from the seed, as every test run does
param (
    [switch] $reseed
)

$compose = "dist/local/docker-compose.yml"

if (-not (Test-Path $compose)) {
    Write-Output "No local build. Run .\bundle.ps1 -environment local first."
    exit 1
}

# The files are bind-mounted, so the containers are recreated to pick up a new build.
docker compose -f $compose up -d --remove-orphans --force-recreate
if ($LASTEXITCODE -ne 0) {
    exit 1
}

$seedOptions = @()
if ($reseed) {
    $seedOptions = @("--fresh")
}
docker compose -f $compose exec -T tt-postgres bash /test-seed/seed.sh @seedOptions
if ($LASTEXITCODE -ne 0) {
    Write-Output "Building the site's database failed."
    exit 1
}
