#!/bin/bash
### Main color palette
for i in 1 2 3 4 5 6 7 8;
do
    printf "\e[1;38;5;${i}m%03d" $i ;
    printf '\e[0m';
done
echo
### Bright colors
for i in 9 10 11 12 13 14 15 16;
do
    printf "\e[1;38;5;${i}m%03d" $i ;
    printf '\e[0m';
done
echo
