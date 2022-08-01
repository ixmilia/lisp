$dateStamp = [System.DateTime]::UtcNow.ToString("yyyyMMddHHmmss")
$versionSuffix = "beta.$dateStamp"
if (Test-Path env:GITHUB_ENV) {
    Write-Output "VERSION_SUFFIX=$versionSuffix" | Out-File -FilePath $env:GITHUB_ENV -Encoding utf8 -Append
}
