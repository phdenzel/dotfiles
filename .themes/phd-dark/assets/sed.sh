#!/bin/sh
sed -i \
         -e 's/#2a2b34/rgb(0%,0%,0%)/g' \
         -e 's/#dde3ee/rgb(100%,100%,100%)/g' \
    -e 's/#31323d/rgb(50%,0%,0%)/g' \
     -e 's/#5f8af7/rgb(0%,50%,0%)/g' \
     -e 's/#383946/rgb(50%,0%,50%)/g' \
     -e 's/#dde3ee/rgb(0%,0%,50%)/g' \
	"$@"
