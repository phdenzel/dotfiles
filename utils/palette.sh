#!/bin/bash
### Main color palette
for i in 232 255 160 29 222 27 125 44 215 105 239;
do
    printf "\e[1;38;5;${i}m%03d" $i ;
    printf '\e[0m';
done
echo
### Bright colors
for i in 232 255 161 35 220 19 198 51 202 57 236;
do
    printf "\e[1;38;5;${i}m%03d" $i ;
    printf '\e[0m';
done
echo
