#!/bin/bash
for((i=0; i<256; i++)); do
    printf "\e[38;5;${i}m%03d" $i;
    printf '\e[0m';
    [ ! $((($i - 15) % 6)) -eq 0 ] && printf ' ' || printf '\n'
done
