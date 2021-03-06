@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0

if NOT exist paket-files (
	powershell write-host -fore Red paket restore
	.paket\paket.exe restore
	if errorlevel 1 (
      exit /b %errorlevel%
    )
)

if NOT exist node_modules (
	powershell write-host -fore Red yarn install
	cmd.exe /c "npm install"
)

if NOT exist paket-files\fable-compiler\Fable\build\fable-library (
	powershell write-host -fore Red build compiler
	cmd.exe /c "npm run build-compiler"
)

cmd.exe /c "npm run %*"