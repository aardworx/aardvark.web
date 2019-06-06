@echo off

set errorlevel=0

git describe --abbrev=0 --tags > tmpFile
if %errorlevel% neq 0 exit /b %errorlevel%
SET /p packageversion= < tmpFile
if %errorlevel% neq 0 exit /b %errorlevel%
DEL tmpFile
if %errorlevel% neq 0 exit /b %errorlevel%

set cont=y
REM powershell Write-Host -NoNewline -ForegroundColor red "create packages  Version: %packageversion%? [Yn]" 
REM if %errorlevel% neq 0 exit /b %errorlevel%
REM set /P cont=""

if /I "%cont%"=="n" (
	powershell Write-Host -ForegroundColor red okay, bye
) else (
	powershell Write-Host -ForegroundColor red creating packages with version %packageversion%
	dotnet pack /p:Version=%packageversion% /p:AssemblyVersion=%packageversion% /p:Authors=Aardworx /p:Copyright=Aardworx /p:RepositoryUrl=https://github.com/aardworx/aardvark.web /p:PackageLicenseExpression=AGPL-3.0-only -o %~dp0dist\ -c Release
	if %errorlevel% neq 0 exit /b %errorlevel%
)
