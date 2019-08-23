@echo off
setlocal

set configuration=Debug

:parseargs
if "%1" == "" goto argsdone
if /i "%1" == "-c" (
    set configuration=%2
    shift
    shift
    goto parseargs
)

echo Unsupported argument: %1
goto error

:argsdone

set SOLUTION=%~dp0IxMilia.Lisp.sln
dotnet restore %SOLUTION%
if errorlevel 1 exit /b 1
dotnet build %SOLUTION% -c %configuration%
if errorlevel 1 exit /b 1
dotnet test %SOLUTION% -c %configuration%
if errorlevel 1 exit /b 1
