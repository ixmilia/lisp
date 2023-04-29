#!/usr/bin/pwsh

[CmdletBinding(PositionalBinding = $false)]
param (
    [string]$configuration = "Debug",
    [string]$versionSuffix = "",
    [switch]$noTest
)

Set-StrictMode -version 2.0
$ErrorActionPreference = "Stop"

function Fail([string]$message) {
    throw $message
}

function Single([string]$pattern) {
    $items = @(Get-Item $pattern)
    if ($items.Length -ne 1) {
        $itemsList = $items -Join "`n"
        Fail "Expected single item, found`n$itemsList`n"
    }

    return $items[0]
}

try {
    # build monaco editor
    Push-Location "$PSScriptRoot\src\lisp-monaco-editor"
    npm i
    npm run compile
    Pop-Location

    # build dotnet
    $solution = Single "$PSScriptRoot/*.sln"
    dotnet restore $solution || Fail "Failed to restore solution"
    dotnet build $solution --configuration $configuration || Fail "Failed to build solution"

    # test
    if (-Not $noTest) {
        dotnet test --no-restore --no-build --configuration $configuration || Fail "Error running tests."
    }

    # create package
    dotnet pack --no-restore --no-build --configuration $configuration /p:VersionSuffix=$versionSuffix $solution || Fail "Error creating package."
    Write-Host "Packages generated at $PSScriptRoot/artifacts/packages/$configuration"

    # create wasm package
    dotnet publish "$PSScriptRoot/src/IxMilia.Lisp.Wasm/IxMilia.Lisp.Wasm.csproj" --configuration $configuration --output "$PSScriptRoot/artifacts/wasm"

    # create vscode extension
    Push-Location "$PSScriptRoot\src\lisp-vscode"
    npm i
    npm run package
    Pop-Location
}
catch {
    Write-Host $_
    Write-Host $_.Exception
    Write-Host $_.ScriptStackTrace
    exit 1
}
