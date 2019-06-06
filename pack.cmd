@echo off

echo %~dp0bin\
dotnet pack /p:Version=1.0.1 /p:AssemblyVersion=1.0.1 /p:Authors=Aardworx /p:Copyright=Aardworx /p:RepositoryUrl=https://github.com/aardworx/aardvark.web /p:PackageLicenseExpression=AGPL-3.0-only -o %~dp0bin\ -c Release