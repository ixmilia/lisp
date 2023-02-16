#!/usr/bin/pwsh

Set-StrictMode -version 2.0
$ErrorActionPreference = "Stop"

try {
    $generatorProject = "$PSScriptRoot\src\IxMilia.Lisp.Generator\IxMilia.Lisp.Generator.csproj"
    $contractsFile = "$PSScriptRoot\src\lisp-monaco-editor\src\grammar\contracts.ts"
    dotnet run --project $generatorProject $contractsFile
}
catch {
    Write-Host $_
    Write-Host $_.Exception
    Write-Host $_.ScriptStackTrace
    exit 1
}
