#!/bin/sh
# run gvfs-info on NestorFormatGroup's FileTestSuite and reformat results as tab separated value
# kramski@dla-marbach.de 22.03.2017, 19.6.2017
# To be run in /cygdrive/?/Workshops/FileTestSuite, having files as ./Misc/*, ./Images/* etc.

printf "Name\tMimetype\n"
find . -mindepth 2 -type f -exec sh -c 'gvfs-info {} | grep "standard::name\|standard::content-type" | cut -f4 -d" " | paste - - ' \;
