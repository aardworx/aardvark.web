@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0

if NOT exist .paket\Paket.Restore.targets (
	powershell write-host -fore Green paket restore
	.paket\paket.exe restore
)

if NOT exist node_modules (
	powershell write-host -fore Green yarn install
	yarn install
)

if NOT exist paket-files\krauthaufen\Fable\build\fable-library (
	powershell write-host -fore Green build compiler
	yarn build-compiler
)

yarn %*