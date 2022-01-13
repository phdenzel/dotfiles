#!/bin/bash


if [[ "$1" == "-sort" || "$1" == "--sort" || "$1" == "-s" ]]; then
    grep "(use-package" "$2" | awk -F 'use-package' '{print $2}' | sort;
else
    grep "(use-package" "$1" | awk -F 'use-package' '{print $2}';
fi;
