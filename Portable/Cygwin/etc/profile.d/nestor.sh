alias ll='ls -la --color=auto'
alias dir='ls -la --color=auto'
alias md='mkdir'

echo "Cygwin -" | figlet
cat << *EOF

Get that Linux feeling - on Windows! (https://cygwin.com/)

Willkommen zur portablen Cygwin-Installation für den nestor-Praktikertag 2017.

Dies ist eine Quick&Dirty-Lösung, um einige interessante Linux-Tools unaufwändig 
unter Windows testen zu können. Für ernsthaftes Arbeiten empfehlen wir eine "echte" 
Linux-Distribution (https://distrochooser.de/) oder zumindest eine korrekte 
Cygwin-Installation (https://cygwin.com/install.html).

Windows-Laufwerksbuchstaben werden in Cygwin als Ordner angesprochen. Laufwerk C: 
z.B. ist /cygdrive/c. Um also etwa eine Datei test.txt auf eimem USB-Stick E: im 
Ordner E:\Temp mit dem Kommando "file" zu inspizieren, geben Sie ein:
   cd /cygdrive/e/Temp
   file test.txt
   
Im Ordner $(cygpath "$DemoFilesPath") haben wir 100 Beispieldateien 
aller Art gesammelt, die Sie untersuchen können.

Sie können dieses Fenster jederzeit mit dem Kommando "logout" schließen.
*EOF

cd $(cygpath "$DemoFilesPath")