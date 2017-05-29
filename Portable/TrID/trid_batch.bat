@echo off
echo Ordner angeben, in denen die Dateien liegen
echo **************************
set /p inputfolder= Ordner :
for /r "%inputfolder%\" %%X in (*.*) do (		
				echo TRId Findings >> govDocs.txt     				
				trid "%%X" >> ergebnis.txt			
				)
Pause
