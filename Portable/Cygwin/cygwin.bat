:: Quasi-"Portable-Cygwin" vom USB-Stick starten
::
:: kramski@dla-marbach.de 22.03.2017

@echo off

setlocal
set MyDrv=%~d0
set MyPath=%~dp0
path %MyPath%\bin;%PATH%

set DemoFilesPath=%MyPath%..\..\Workshops\FileTestSuite

%MyDrv%
cd %MyPath%\bin

:: Start bash as login shell:
mintty.exe -s 96,30 -i /Cygwin-Terminal.ico -

if errorlevel 1 goto ERROR
goto EXIT

:ERROR
:: Moep...

:EXIT
endlocal

