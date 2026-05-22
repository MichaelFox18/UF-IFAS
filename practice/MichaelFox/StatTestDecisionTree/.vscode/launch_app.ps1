# StatGuide launcher — finds Rscript.exe automatically even if R is not on PATH

$rscript = $null

# 1. Check PATH first
$fromPath = Get-Command "Rscript.exe" -ErrorAction SilentlyContinue
if ($fromPath) { $rscript = $fromPath.Source }

# 2. Scan common install locations
if (-not $rscript) {
    $searchRoots = @(
        "C:\Program Files\R",
        "C:\Program Files (x86)\R",
        "$env:LOCALAPPDATA\Programs\R",
        "$env:APPDATA\R"
    )
    foreach ($root in $searchRoots) {
        if (Test-Path $root) {
            $found = Get-ChildItem $root -Recurse -Filter "Rscript.exe" -Depth 4 -ErrorAction SilentlyContinue |
                     Sort-Object FullName -Descending | Select-Object -First 1
            if ($found) { $rscript = $found.FullName; break }
        }
    }
}

# 3. Check registry
if (-not $rscript) {
    $regKeys = @(
        "HKLM:\SOFTWARE\R-core\R",
        "HKLM:\SOFTWARE\WOW6432Node\R-core\R",
        "HKCU:\SOFTWARE\R-core\R"
    )
    foreach ($key in $regKeys) {
        try {
            $installPath = (Get-ItemProperty $key -ErrorAction Stop).InstallPath
            $candidate   = Join-Path $installPath "bin\Rscript.exe"
            if (Test-Path $candidate) { $rscript = $candidate; break }
        } catch {}
    }
}

if (-not $rscript) {
    Write-Host ""
    Write-Host "R not found on this machine." -ForegroundColor Red
    Write-Host ""
    Write-Host "Please install R first:" -ForegroundColor Yellow
    Write-Host "  https://cran.r-project.org/bin/windows/base/" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "After installing, re-run this script or use Terminal > Run Task > 'R: Run StatGuide App'." -ForegroundColor Yellow
    exit 1
}

Write-Host "Using R: $rscript" -ForegroundColor Green
Write-Host "Starting StatGuide — the app will open in your browser..." -ForegroundColor Cyan
Write-Host ""

$appDir = Split-Path -Parent $PSScriptRoot
Set-Location $appDir
& $rscript "run_app.R"
