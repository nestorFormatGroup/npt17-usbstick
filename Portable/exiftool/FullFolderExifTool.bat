@echo off
echo Ordner angeben, in denen Bilddateien liegen
echo **************************
rem Ordner angeben
set /p inputfolder= Ordner :
exiftool -api validate -a -u -G1 -r -csv "%inputfolder%" > auswertung.csv
pause


