@echo off

set errorlevel=0

git describe --abbrev=0 --tags > tmpFile
if %errorlevel% neq 0 exit /b %errorlevel%
SET /p packageversion= < tmpFile
if %errorlevel% neq 0 exit /b %errorlevel%
DEL tmpFile
if %errorlevel% neq 0 exit /b %errorlevel%

SET /p key= < %HOMEPATH%\.ssh\aardworx.key
if %errorlevel% neq 0 exit /b %errorlevel%
echo %key%

powershell Write-Host -ForegroundColor red creating packages with version %packageversion%
REM dotnet pack /p:Version=%packageversion% /p:AssemblyVersion=%packageversion% /p:Authors=Aardworx /p:Copyright=Aardworx /p:RepositoryUrl=https://github.com/aardworx/aardvark.web /p:PackageLicenseExpression=AGPL-3.0-only -o %~dp0dist\ -c Release
if %errorlevel% neq 0 exit /b %errorlevel%
