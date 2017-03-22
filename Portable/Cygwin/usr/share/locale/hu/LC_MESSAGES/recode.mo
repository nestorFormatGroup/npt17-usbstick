��    :      �  O   �      �  �  �  �   �  8  F  �  	  0   3  9   d  6   �  C   �                >     ]     q     z     �     �  '   �  $   �  $   �  $        =     [     u     �     �     �  H   �          -     F     a     j  "   |     �     �     �     �     �          !     :     I  (   `     �  �   �  �  9  "   �     �          -     B     [  7   u     �  
   �     �     �  |  �  H  H  �   �  �  D  �  �  @   �  R   �  A   >  ^   �     �  (   �  )         ;      V      _      l      �   1   �   <   �   3   !  9   C!     }!  "   �!  %   �!     �!     "      "  \   8"      �"     �"  2   �"  
   #      #  >   .#     m#     �#     �#     �#  &   �#  "   �#     $     ($     ?$  2   V$     �$  �   �$  ;  ?%  1   {(     �(     �(     �(     )      )  4   @)     u)     z)     �)     �)         -                                                                   2       /      5   7           &       *   6   %   9   	   #                  !   "      ,             )   4                  1   :      $      +   
   (                   0   3      8         .       '           
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
Report bugs to <recode-bugs@iro.umontreal.ca>.
 
Usage: %s [OPTION]... [ [CHARSET] | REQUEST [FILE]... ]
   -p, --sequence=pipe     same as -i (on this system)
   -p, --sequence=pipe     use pipe machinery for sequencing passes
  done
  failed: %s in step `%s..%s'
 %s failed: %s in step `%s..%s' %s in step `%s..%s' %s to %s *Unachievable* *mere copy* Ambiguous output Charset %s already exists and is not %s Charset `%s' is unknown or ambiguous Child process wait status is 0x%0.2x Codes %3d and %3d both recode to %3d Dec  Oct Hex   UCS2  Mne  %s
 Expecting `..' in request Format `%s' is ambiguous Format `%s' is unknown Internal recoding bug Invalid input LN is some language, it may be `c', `perl' or `po'; `c' is the default.
 Language `%s' is ambiguous Language `%s' is unknown Misuse of recoding library No error No table to print No way to recode from `%s' to `%s' Non canonical input Recoding %s... Request `%s' is erroneous Request: %s
 Required argument is missing Sequence `%s' is ambiguous Sequence `%s' is unknown Shrunk to: %s
 Symbol `%s' is unknown Syntax is deprecated, please prefer `%s' System detected problem This is free software; see the source for copying conditions.  There is NO
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

 Unrecognised surface name `%s' Untranslatable input Virtual memory exhausted Virtual memory exhausted! Written by Franc,ois Pinard <pinard@iro.umontreal.ca>.
 byte reversible ucs2 variable Project-Id-Version: recode 3.6
Report-Msgid-Bugs-To: recode-bugs@iro.umontreal.ca
POT-Creation-Date: 2008-03-09 20:51-0400
PO-Revision-Date: 2003-03-02 18:08+0100
Last-Translator: Andras Timar <timar_a@freemail.hu>
Language-Team: Hungarian <translation-team-hu@lists.sourceforge.net>
MIME-Version: 1.0
Content-Type: text/plain; charset=iso-8859-2
Content-Transfer-Encoding: 8-bit
 
Finomhangol�s:
  -s, --strict           szigor� lek�pez�s haszn�lata, ak�r karakterveszt�s 
                         �r�n is
  -d, --diacritics       csak a nemzeti karaktereket k�dolja �t HTML/LaTeX 
                         form�tumhoz
  -S, --source[=NYELV]   csak a NYELV �ltal meghat�rozott karakterl�ncokat
                         �s megjegyz�seket k�dolja �t
  -c, --colons           `:' haszn�lata a `"' helyett a k�tpontos �kezet 
                         jel�l�s�re az Easy French (egyfajta rep�l� �kezet)
                         k�dol�sban
  -g, --graphics         az IBMPC vonalrajzol� karakterek helyettes�t�se
                         ASCII grafik�val
  -x, --ignore=KARK�SZL  a KARK�SZL mell�z�se �tk�dol�si �tvonal v�laszt�s�n�l
 
Ha a hossz� kapcsol�hoz k�telez� argumentumot �rni, akkor az a megfelel�
r�vid kapcsol�ra is vonatkozik. Hasonl� a helyzet a v�laszthat� 
argumentumokkal.
 
List�k:
  -l, --list[=FORM�TUM]      ki�rja az �sszes ismert karakterk�szletet
  -k, --known=PAIRS          restrict charsets according to known PAIRS list
  -h, --header[=[LN/]NAME]   write table NAME on stdout using LN, then exit
  -F, --freeze-tables        write out a C module holding all tables
  -T, --find-subsets         ki�rja azokat a karakterk�szleteket, amelyeket 
                             m�sok magukba foglalnak
  -C, --copyright            ki�rja a copyrightot �s a m�sol�s felt�teleit
      --help                 ki�rja ezt a s�g�t �s kil�p
      --version              ki�rja a verzi�inform�ci�kat �s kil�p
 
M�k�d�si m�dok:
  -v, --verbose           ki�rja a l�p�seket �s az el�rehalad�st
  -q, --quiet, --silent   nem �r ki �zeneteket visszaford�thatatlan
                          �tk�dol�s eset�n
  -f, --force             �tk�dol�s k�nyszer�t�se visszaford�thatatlan 
                          esetben is
  -t, --touch             az �tk�dolt f�jlok d�tum�t m�dos�tja
  -i, --sequence=files    k�zb�ls� f�jlokat haszn�l az egym�st k�vet� 
                          l�p�sekhez
      --sequence=memory   mem�ri�ban elhelyezked� t�rol�kat haszn�l az 
                          egym�st k�vet� l�p�sekhez
 
A hib�kat jelentsd a <recode-bugs@iro.umontreal.ca> c�men.
 
Haszn�lat: %s [KAPCSOL�K]... [ [KARAKTERK�SZLET] | K�R�S [F�JL]... ]
   -p, --sequence=pipe     ugyanaz, mint a -i (ezen a rendszeren)
   -p, --sequence=pipe     cs�vezet�keket haszn�l az egym�st k�vet� l�p�sekhez
  k�sz
  sikertelen: %s a `%s..%s' l�p�sben
 %s sikertelen: %s a `%s..%s' l�p�sben %s a `%s..%s' l�p�sben %s -> %s *lehetetlen* *egyszer� m�sol�s* Nem egy�rtelm� kimenet %s karakterk�szlet m�r l�tezik �s nem %s. A karakterk�szlet ismeretlen vagy nem egy�rtelm�: `%s' A gyermekfolyamat v�rakoz�si �llapota 0x%0.2x A %3d �s %3d k�dok egyar�nt %3d k�dra v�ltoznak Dec  Oct Hex   UCS2  Mne  %s
 `..'-t kell megadni a k�r�sben A form�tum nem egy�rtelm�: `%s' A form�tum ismeretlen: `%s' Bels� �tk�dol�si hiba �rv�nytelen bemenet A NYELV valamilyen programnyelv, lehet `c', `perl' vagy `po'; a `c' az alap-
�rtelmezett.
 A nyelv nem egy�rtelm�: `%s' A nyelv ismeretlen: `%s' Az �tk�dol�k�nyvt�r t�ves haszn�lata Hib�tlan Nincs nyomtathat� t�bl�zat Nem lehets�ges az �tk�dol�s `%s' �s `%s' k�z�tt. Nem kanonikus bemenet %s �tk�dol�sa... A k�r�s hib�s: `%s' K�r�s: %s
 Hi�nyzik egy k�telez� argumentum A sorozat nem egy�rtelm�: `%s' A sorozat ismeretlen: `%s' Sz�k�tve erre: %s
 A jel ismeretlen: `%s' A szintaxis elavult, haszn�ld ink�bb ezt: `%s' Rendszerhiba Ez szabad szoftver; l�sd a forr�st a terjeszt�si felt�telek�rt. Nincs 
garancia, m�g az eladhat�s�gra vagy egy adott c�lra val� megfelel�sre sem.
 Ez a program szabad szoftver; terjeszthet� �s/vagy m�dos�that� a 
Free Software Foundation �ltal k�zz�tett GNU General Public License 
felt�teleinek megfelel�en. A licencszerz�d�s 2. vagy (szabadon
v�laszthat�an) b�rmelyik k�s�bbi v�ltozata haszn�lhat�.

A programot abban a rem�nyben adjuk k�zre, hogy hasznosnak bizonyul,
de NINCS SEMMI GARANCIA; bele�rtve a K�ZREADHAT�S�GGAL vagy EGY
BIZONYOS C�LRA VAL� ALKALMASS�GGAL kapcsolatos garanci�t is. Tov�bbi
r�szletek�rt olvasd el a GNU General Public License teljes sz�veg�t.

A programmal egy�tt meg kellett kapnod a GNU General Public License
egy p�ld�ny�t. Ha nem kaptad meg, �rj a Free Software Foundation-nek
a k�vetkez� c�mre: 
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 Tov�bbi inform�ci��rt �rd be: `%s %s'.
 UCS2   Mne   le�r�s

 Ismeretlen fel�letn�v `%s' Leford�thatatlan bemenet A virtu�lis mem�ria betelt A virtu�lis mem�ria betelt! �rta Fran�ois Pinard <pinard@iro.umontreal.ca>.
 byte visszaford�that� ucs2 v�ltoz� 