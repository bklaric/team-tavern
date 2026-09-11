<#
.SYNOPSIS
    Lists the open, unblocked issues in the local markdown tracker.

.DESCRIPTION
    Reads every specs/<feature>/issues/NN-<slug>.md, takes the Status and
    Blocked by lines from the header (everything above the first ## heading),
    and prints the tickets that are open and whose blockers have all resolved.
    Blocker numbers name siblings in the same issues directory. Only 'resolved'
    unblocks: a 'wontfix' blocker still holds its dependents back, because
    dropping a ticket does not answer the question the dependent was waiting on.

.PARAMETER All
    Print every open ticket, blocked ones included, with what each waits on.

.EXAMPLE
    .\issues.ps1
    .\issues.ps1 -All
#>
[CmdletBinding()]
param(
    [string] $SpecsPath = (Join-Path $PSScriptRoot 'specs'),
    [switch] $All
)

$ErrorActionPreference = 'Stop'

$ClosedStates = @('resolved', 'wontfix')
$KnownStates = @('needs-triage', 'needs-info', 'ready-for-agent', 'ready-for-human') + $ClosedStates

function Get-IssueHeader {
    param([string] $Path)

    $header = @()
    foreach ($line in (Get-Content -LiteralPath $Path)) {
        if ($line -match '^##\s') { break }
        $header += $line
    }
    return $header
}

function Read-Issue {
    param([System.IO.FileInfo] $File)

    $status = ''
    $blockers = @()
    foreach ($line in (Get-IssueHeader -Path $File.FullName)) {
        if (-not $status -and $line -match '^Status:\s*(\S+)') {
            $status = $Matches[1]
        }
        if ($line -match '^Blocked by:\s*(.+)$') {
            $blockers += $Matches[1] -split ',' |
                ForEach-Object { $_.Trim() } |
                Where-Object { $_ }
        }
    }

    $number = ''
    if ($File.Name -match '^(\d+)-') { $number = $Matches[1] }

    [PSCustomObject]@{
        Feature  = $File.Directory.Parent.Name
        Number   = $number
        Slug     = $File.BaseName
        Status   = $status
        Blockers = $blockers
        Path     = $File.FullName
    }
}

function Get-UnresolvedBlockers {
    param($Issue, [hashtable] $Index)

    $waiting = @()
    foreach ($number in $Issue.Blockers) {
        $key = "$($Issue.Feature)/$number"
        if (-not $Index.ContainsKey($key)) {
            $waiting += "$number (no such ticket)"
        } elseif ($Index[$key].Status -ne 'resolved') {
            $waiting += $number
        }
    }
    return $waiting
}

if (-not (Test-Path -LiteralPath $SpecsPath)) {
    Write-Error "No specs directory at $SpecsPath"
}

$root = (Get-Item -LiteralPath $SpecsPath).Parent.FullName

$issues = Get-ChildItem -LiteralPath $SpecsPath -Recurse -File -Filter '*.md' |
    Where-Object { $_.Directory.Name -eq 'issues' } |
    ForEach-Object { Read-Issue -File $_ }

$index = @{}
foreach ($issue in $issues) {
    if ($issue.Number) { $index["$($issue.Feature)/$($issue.Number)"] = $issue }
}

foreach ($issue in $issues) {
    if (-not $issue.Status) {
        Write-Warning "No Status line: $($issue.Feature)/$($issue.Slug)"
    } elseif ($KnownStates -notcontains $issue.Status) {
        Write-Warning "Unrecognised status '$($issue.Status)': $($issue.Feature)/$($issue.Slug)"
    }
}

$rows = $issues |
    Where-Object { $ClosedStates -notcontains $_.Status } |
    Sort-Object Feature, Number |
    ForEach-Object {
        $waiting = Get-UnresolvedBlockers -Issue $_ -Index $index
        [PSCustomObject]@{
            Issue     = "$($_.Feature)/$($_.Slug)"
            Status    = $(if ($_.Status) { $_.Status } else { '(none)' })
            BlockedBy = $waiting -join ', '
            Path      = $_.Path.Substring($root.Length + 1).Replace([string][char]92, '/')
        }
    }

if (-not $All) {
    $rows = $rows | Where-Object { -not $_.BlockedBy }
}

if (-not $rows) {
    if ($All) { Write-Host 'No open issues.' } else { Write-Host 'No open issue is unblocked.' }
    return
}

if ($All) {
    $rows | Format-Table Issue, Status, BlockedBy, Path -AutoSize
} else {
    $rows | Format-Table Issue, Status, Path -AutoSize
}
