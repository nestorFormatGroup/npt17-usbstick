��    M      �  g   �      �  �  �  �   E  �   �  8  �	  �  �  �   �  |  "  0   �  9   �  6   
  C   A     �     �     �     �     �     �     �     
       -   '  $   U  5   z  '   �  $   �  $   �  $   "     G     e  &        �     �  $   �     �       H        h     �     �     �     �     �  "   �       1   %     W  (   f     �     �     �  &   �     �          .  "   =     `  0   {     �  (   �     �  �     �  �  "   3     V  �   q     �  &        ?     T     m  �   �  7   \     �  
   �     �     �  c  �       �     &  �  �  �   �  �#  �   �%  �  U&  4   �'  D   .(  <   s(  <   �(     �(     �(     )     2)     G)     O)     h)     t)     �)  G   �)  ,   �)  R   *  .   d*  4   �*  /   �*  0   �*     )+     G+  ,   g+     �+     �+  )   �+     �+     ,  S   #,     w,     �,  -   �,  #   �,     -      -  $   6-     [-  6   u-     �-  9   �-     �-     .     #.  7   @.     x.     �.     �.  /   �.      �.  8   /     N/  .   m/  !   �/  �   �/  �  h0  %   73     ]3  �   x3  #   -4  7   Q4     �4     �4     �4  �   �4  7   �5     �5  
    6     6     6                   >   $                  ;   5   0         1      B         H                K   *   C                    7          ?          :       @      I       "   2   =       3   	   A   E   /   J      G       <       4   ,          F              8   %                     (   )           L            #           '      +       !   &      9   
   6   D   -      M   .        
Fine tuning:
  -s, --strict           use strict mappings, even loose characters
  -d, --diacritics       convert only diacritics or alike for HTML/LaTeX
  -S, --source[=LN]      limit recoding to strings and comments as for LN
  -c, --colons           use colons instead of double quotes for diaeresis
  -g, --graphics         approximate IBMPC rulers by ASCII graphics
  -x, --ignore=CHARSET   ignore CHARSET while choosing a recoding path
 
If a long option shows an argument as mandatory, then it is mandatory
for the equivalent short option also.  Similarly for optional arguments.
 
If none of -i and -p are given, presume -p if no FILE, else -i.
Each FILE is recoded over itself, destroying the original.  If no
FILE is specified, then act as a filter and recode stdin to stdout.
 
Listings:
  -l, --list[=FORMAT]        list one or all known charsets and aliases
  -k, --known=PAIRS          restrict charsets according to known PAIRS list
  -h, --header[=[LN/]NAME]   write table NAME on stdout using LN, then exit
  -F, --freeze-tables        write out a C module holding all tables
  -T, --find-subsets         report all charsets being subset of others
  -C, --copyright            display Copyright and copying conditions
      --help                 display this help and exit
      --version              output version information and exit
 
Operation modes:
  -v, --verbose           explain sequence of steps and report progress
  -q, --quiet, --silent   inhibit messages about irreversible recodings
  -f, --force             force recodings even when not reversible
  -t, --touch             touch the recoded files after replacement
  -i, --sequence=files    use intermediate files for sequencing passes
      --sequence=memory   use memory buffers for sequencing passes
 
Option -l with no FORMAT nor CHARSET list available charsets and surfaces.
FORMAT is `decimal', `octal', `hexadecimal' or `full' (or one of `dohf').
 
REQUEST is SUBREQUEST[,SUBREQUEST]...; SUBREQUEST is ENCODING[..ENCODING]...
ENCODING is [CHARSET][/[SURFACE]]...; REQUEST often looks like BEFORE..AFTER,
with BEFORE and AFTER being charsets.  An omitted CHARSET implies the usual
charset; an omitted [/SURFACE]... means the implied surfaces for CHARSET; a /
with an empty surface name means no surfaces at all.  See the manual.
 
Report bugs to <recode-bugs@iro.umontreal.ca>.
 
Usage: %s [OPTION]... [ [CHARSET] | REQUEST [FILE]... ]
   -p, --sequence=pipe     same as -i (on this system)
   -p, --sequence=pipe     use pipe machinery for sequencing passes
  done
  failed: %s in step `%s..%s'
 %s failed: %s in step `%s..%s' %s in step `%s..%s' %s to %s %sfor sequence %s.%s *Unachievable* *mere copy* Ambiguous output Cannot complete table from set of known pairs Cannot invert given one-to-one table Cannot list `%s', no names available for this charset Charset %s already exists and is not %s Charset `%s' is unknown or ambiguous Child process wait status is 0x%0.2x Codes %3d and %3d both recode to %3d Dec  Oct Hex   UCS2  Mne  %s
 Expecting `..' in request Following diagnostics for `%s' to `%s' Format `%s' is ambiguous Format `%s' is unknown Identity recoding, not worth a table Internal recoding bug Invalid input LN is some language, it may be `c', `perl' or `po'; `c' is the default.
 Language `%s' is ambiguous Language `%s' is unknown Misuse of recoding library No character recodes to %3d No error No table to print No way to recode from `%s' to `%s' Non canonical input Pair no. %d: <%3d, %3d> conflicts with <%3d, %3d> Recoding %s... Recoding is too complex for a mere table Request `%s' is erroneous Request: %s
 Required argument is missing Resurfacer set more than once for `%s' Sequence `%s' is ambiguous Sequence `%s' is unknown Shrunk to: %s
 Sorry, no names available for `%s' Step initialisation failed Step initialisation failed (unprocessed options) Symbol `%s' is unknown Syntax is deprecated, please prefer `%s' System detected problem This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 Try `%s %s' for more information.
 UCS2   Mne   Description

 Unless DEFAULT_CHARSET is set in environment, CHARSET defaults to the locale
dependent encoding, determined by LC_ALL, LC_CTYPE, LANG.
 Unrecognised surface name `%s' Unsurfacer set more than once for `%s' Untranslatable input Virtual memory exhausted Virtual memory exhausted! With -k, possible before charsets are listed for the given after CHARSET,
both being tabular charsets, with PAIRS of the form `BEF1:AFT1,BEF2:AFT2,...'
and BEFs and AFTs being codes are given as decimal numbers.
 Written by Franc,ois Pinard <pinard@iro.umontreal.ca>.
 byte reversible ucs2 variable Project-Id-Version: Free recode 3.6
Report-Msgid-Bugs-To: recode-bugs@iro.umontreal.ca
POT-Creation-Date: 2008-03-09 20:51-0400
PO-Revision-Date: 2002-09-17 00:28+0200
Last-Translator: Santiago Vila Doncel <sanvila@unex.es>
Language-Team: Spanish <es@li.org>
MIME-Version: 1.0
Content-Type: text/plain; charset=ISO-8859-1
Content-Transfer-Encoding: 8-bit
 
Ajuste `fino':
  -s, --strict           utiliza correspondencias estrictas, aunque se pierdan
                         caracteres
  -d, --diacritics       convierte solamente acentos y dem�s para HTML/LaTeX
  -S, --source[=LN]      limita la conversi�n a cadenas y comentarios seg�n LN
  -c, --colons           utiliza dos puntos en vez de comillas para di�resis
  -g, --graphics         aproxima gr�ficos IBMPC mediante gr�ficos ASCII
  -x, --ignore=JUEGO     descarta JUEGO al escoger un camino de conversi�n
 
Si una opci�n larga muestra un argumento como obligatorio, entonces es
obligatorio tambi�n para la opci�n corta equivalente. Lo mismo para los
argumentos opcionales.
 
Si no se especifica ninguna de las opciones -i � -p, se supone -p si no
hay ning�n FICHERO, o si no, -i. Cada FICHERO se convierte sobre s� mismo,
destruyendo el original. Si no es especifica ning�n FICHERO, entonces funciona
como filtro y convierte la entrada est�ndar en la salida est�ndar.
 
Listados:
  -l, --list[=FORMATO]       muestra uno o todos los juegos y alias conocidos
  -k, --known=PARES          restringe los juegos de acuerdo con la lista de
                             PARES conocidos
  -h, --header[=[LN/]NOMBRE  escribe la tabla NOMBRE en la salida est�ndar
                             usando LN, y finaliza
  -F, --freeze-tables        escribe un m�dulo en C con todas las tablas
  -T, --find-subsets         informa acerca de todos los juegos de caracteres
                             que son subconjuntos de otros
  -C, --copyright            muestra el Copyright y las condiciones de copia
      --help                 muestra esta ayuda y finaliza
      --version              muestra la versi�n y finaliza
 
Modos de operaci�n:
  -v, --verbose           explica la sucesi�n de pasos e informa del desarrollo
  -q, --quiet, --silent   no muestra mensajes sobre conversiones irreversibles
  -f, --force             realiza la conversi�n incluso si no es reversible
  -t, --touch             toca los ficheros convertidos despu�s del reemplazo
  -i, --sequence=files    utiliza ficheros intermedios para las pasadas
      --sequence=memory   utiliza b�fers en memoria para las pasadas
 
La opci�n -l sin FORMATO ni JUEGO muestra los juegos de caracteres y facetas.
disponibles. FORMATO es `decimal', `octal', `hexadecimal' o `full' (o uno de
entre `dohf').
 
PETICI�N es SUBPETICI�N[,SUBPETICI�N]...;
SUBPETICI�N es CODIFICACI�N[..CODIFICACI�N]...
CODIFICACI�N es [JUEGO][/[FACETA]]...; PETICI�N suele ser ANTES..DESPU�S,
donde ANTES y DESPU�S son juegos de caracteres.  Un JUEGO omitido implica el
juego de caracteres usual; una [/FACETA]... omitida significa la faceta
impl�cita para el JUEGO; una / con una faceta vac�a significa que no hay
ninguna faceta. V�ase el manual.
 
Comunicar bichos a <recode-bugs@iro.umontreal.ca>.
 
Modo de empleo: %s [OPCI�N]... [ [JUEGO] | PETICI�N [FICHERO]... ]
   -p, --sequence=pipe     lo mismo que -i (en este sistema)
   -p, --sequence=pipe     utiliza tuber�as para las pasadas
  hecho
  fall�: %s en el paso %s..%s
 %s fall�: %s en el paso %s..%s %s en el paso %s..%s %s a %s %spara la sucesi�n %s.%s *Imposible* *copia pura y simple* Resultado ambiguo No se puede completar la tabla a partir del conjunto de pares conocidos No se puede invertir la tabla uno-a-uno dada No se puede mostrar `%s', no hay nombres disponibles para este
juego de caracteres El juego de caracteres %s ya existe y no es %s El juego de caracteres `%s' es desconocido o ambiguo El estado de espera del proceso hijo es 0x%0.2x Los c�digos %3d y %3d se convierten ambos en %3d Dec  Oct Hex   UCS2  Nem  %s
 Se esperaba `..' en la petici�n Procedimiento para convertir de `%s' a `%s': El formato `%s' es ambiguo El formato `%s' es desconocido Conversi�n identidad, no merece una tabla Bug interno de conversi�n Entrada inv�lida LN es alg�n lenguaje, puede ser `c', `perl' o `po', el valor predeterminado
es `c'
 El idioma `%s' es ambiguo El idioma `%s' es desconocido Uso inadecuado de la biblioteca de conversi�n Ning�n car�cter se convierte en %3d No hay ning�n error No hay ninguna tabla que mostrar No se puede convertir de `%s' a `%s' La entrada no es can�nica El par n� %d: <%3d, %3d> es conflictivo con <%3d, %3d> Convirtiendo %s... La conversi�n es demasiado compleja para una simple tabla La petici�n `%s' es err�nea Petici�n: %s
 Falta el argumento requerido Se ha establecido `resurfacer' m�s de una vez para `%s' La sucesi�n `%s' es ambigua La sucesi�n `%s' es desconocida Encogido a: %s
 Lo siento, no hay nombres disponibles para `%s' Fall� la etapa de inicializaci�n Fall� la etapa de inicializaci�n (opciones sin procesar) El s�mbolo `%s' es desconocido La sintaxis es antigua, por favor utilice `%s' Problema detectado por el sistema Esto es software libre; vea el c�digo fuente para las condiciones de copia.
No hay NINGUNA garant�a; ni siquiera de COMERCIABILIDAD o IDONEIDAD PARA UN
FIN DETERMINADO.
 Este programa es software libre; puede ser redistribuido y/o
modificado bajo los t�rminos de la Licencia P�blica General de
GNU tal y como se publica por la Free Software Foundation; bien
en su versi�n 2, o (a su elecci�n) cualquier versi�n posterior.

Este programa se distribuye con la esperanza de que sea �til,
pero SIN NINGUNA GARANT�A; ni siquiera la garant�a impl�cita de
COMERCIABILIDAD o IDONEIDAD PARA UN FIN DETERMINADO. V�ase la
Licencia P�blica General de GNU para m�s detalles.

Usted deber�a haber recibido una copia de la Licencia P�blica
General de GNU junto con este programa; en caso contrario, escriba
a la Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, EE.UU.
 Pruebe `%s %s' para m�s informaci�n.
 UCS2   Nem   Descripci�n

 A menos que se establezca DEFAULT_CHARSET en el entorno, el valor
predeterminado de CHARSET es la codificaci�n de la que depende el locale,
determinada por LC_ALL, LC_CTYPE, LANG.
 Nombre de faceta no reconocido `%s' Se ha establecido `unsurfacer' m�s de una vez para `%s' Entrada no convertible Memoria virtual agotada �Memoria virtual agotada! Con -k, se muestran los posibles juegos `antes' para el JUEGO `despu�s' dado,
siendo ambos juegos de caracteres tabulares, con PAREJAS de la forma
`ANT1:DES1,ANT2:DES2,...' y siendo los ANTs y los DESs c�digos dados
como n�meros decimales.
 Escrito por Fran�ois Pinard <pinard@iro.umontreal.ca>.
 byte reversible ucs2 variable 