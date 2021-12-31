#!/bin/bash

IFS=:
for SET in $LS_COLORS
do
    TYPE=$(echo $SET | cut -d"=" -f1)
    COLOUR=$(echo $SET | cut -d"=" -f2)

    case $TYPE in
        no) TEXT="Global default";;
        fi) TEXT="Normal file";;
        di) TEXT="Directory";;
        ln) TEXT="Symbolic link";;
        pi) TEXT="Named pipe";;
        so) TEXT="Socket";;
        do) TEXT="Door";;
        bd) TEXT="Block device";;
        cd) TEXT="Character device";;
        or) TEXT="Orphaned symbolic link";;
        mi) TEXT="Missing file";;
        su) TEXT="Set UID";;
        sg) TEXT="Set GID";;
        tw) TEXT="Sticky other writable";;
        ow) TEXT="Other writable";;
        st) TEXT="Sticky";;
        ex) TEXT="Executable";;
        rs) TEXT="Reset to \"normal\" color";;
        mh) TEXT="Multi-Hardlink";;
        ca) TEXT="File with capability";;
        *) TEXT="${TYPE} (TODO: get description)";;
    esac

    printf "Type: %-10s Colour: %-10s \e[${COLOUR}m${TEXT}\e[0m\n" "${TYPE}" "${COLOUR}"
done
