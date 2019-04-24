@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0

if NOT exist .paket\Paket.Restore.targets (
	powershell write-host -fore Red paket restore
	.paket\paket.exe restore
	if errorlevel 1 (
      exit /b %errorlevel%
    )
)

if NOT exist node_modules (
	powershell write-host -fore Red yarn install
	cmd.exe /c "yarn install"
)

if NOT exist paket-files\krauthaufen\Fable\build\fable-library (
	powershell write-host -fore Red build compiler
	cmd.exe /c "yarn build-compiler"
)

cmd.exe /c "yarn %*"