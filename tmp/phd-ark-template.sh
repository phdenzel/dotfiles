### #phd-ark-capitalize color palette
# Note: These variables are not exported! The color variables are restricted to
#       the current shell only, and have to be sourced with each new shell session.
if tput setaf 1 &> /dev/null; then
    tput sgr0; # reset
    bold=$(tput bold);
    underline=$(tput smul);
    underline_reset=$(tput rmul);
    reset=$(tput sgr0);
    # Custom color palette (see .dircolors)
    crust=$(tput setaf #phd-ark-crust/b256);
    red=$(tput setaf #phd-ark-red/b256);
    green=$(tput setaf #phd-ark-green/b256);
    yellow=$(tput setaf #phd-ark-yellow/b256);
    blue=$(tput setaf #phd-ark-blue/b256);
    pink=$(tput setaf #phd-ark-pink/b256);
    teal=$(tput setaf #phd-ark-teal/b256);
    subtext0=$(tput setaf #phd-ark-subtext0/b256);
    surface1=$(tput setaf #phd-ark-surface1/b256);
    ruby=$(tput setaf #phd-ark-ruby/b256);
    viridis=$(tput setaf #phd-ark-viridis/b256);
    sand=$(tput setaf #phd-ark-sand/b256);
    indigo=$(tput setaf #phd-ark-indigo/b256);
    magenta=$(tput setaf #phd-ark-magenta/b256);
    cyan=$(tput setaf #phd-ark-cyan/b256);
    white=$(tput setaf #phd-ark-white/b256);

    crimson=$(tput setaf #phd-ark-crimson/b256);
    tiger=$(tput setaf #phd-ark-tiger/b256);
    emerald=$(tput setaf #phd-ark-emerald/b256);
    lilac=$(tput setaf #phd-ark-lilac/b256);
    purple=$(tput setaf #phd-ark-purple/b256);

    orange=$(tput setaf #phd-ark-orange/t256);
    grass=$(tput setaf #phd-ark-grass/t256);
    ocean=$(tput setaf #phd-ark-ocean/t256);
    turquoise=$(tput setaf #phd-ark-turquoise/t256);
    amethyst=$(tput setaf #phd-ark-amethyst/t256);
    violet=$(tput setaf #phd-ark-violet/t256);
    surface0=$(tput setaf #phd-ark-surface0/t256);
    subtext1=$(tput setaf #phd-ark-subtext1/t256);
    textcolor=$(tput setaf #phd-ark-text/t256);
    overlay2=$(tput setaf #phd-ark-overlay2/t256);
    surface2=$(tput setaf #phd-ark-surface2/t256);
else
    crust="\e[1;38;5;#phd-ark-crust/b256m";
    red="\e[1;38;5;#phd-ark-red/b256m";
    green="\e[1;38;5;#phd-ark-green/b256m";
    yellow="\e[1;38;5;#phd-ark-yellow/b256m";
    blue="\e[1;38;5;#phd-ark-blue/b256m";
    pink="\e[1;38;5;#phd-ark-pink/b256m";
    teal="\e[1;38;5;#phd-ark-teal/b256m";
    subtext0="\e[1;38;5;#phd-ark-subtext0/b256m";
    surface1="\e[1;38;5;#phd-ark-surface1/b256m";
    ruby="\e[1;38;5;#phd-ark-ruby/b256m";
    viridis="\e[1;38;5;#phd-ark-viridis/b256m";
    sand="\e[1;38;5;#phd-ark-sand/b256m";
    indigo="\e[1;38;5;#phd-ark-indigo/b256m";
    magenta="\e[1;38;5;#phd-ark-magenta/b256m";
    cyan="\e[1;38;5;#phd-ark-cyan/b256m";
    white="\e[1;38;5;#phd-ark-white/b256m";

    crimson="\e[1;38;5;#phd-ark-crimson/b256m";
    tiger="\e[1;38;5;#phd-ark-tiger/b256m";
    emerald="\e[1;38;5;#phd-ark-emerald/b256m";
    lilac="\e[1;38;5;#phd-ark-lilac/b256m";
    purple="\e[1;38;5;#phd-ark-purple/b256m";

    orange="\e[1;38;5;#phd-ark-orange/t256m";
    grass="\e[1;38;5;#phd-ark-grass/t256m";
    ocean="\e[1;38;5;#phd-ark-ocean/t256m";
    turquoise="\e[1;38;5;#phd-ark-turquoise/t256m";
    amethyst="\e[1;38;5;#phd-ark-amethyst/t256m";
    violet="\e[1;38;5;#phd-ark-violet/t256m";
    surface0="\e[1;38;5;#phd-ark-surface0/t256m";
    subtext1="\e[1;38;5;#phd-ark-subtext1/t256m";
    textcolor="\e[1;38;5;#phd-ark-text/t256m";
    overlay2="\e[1;38;5;#phd-ark-overlay2/t256m";
    surface2="\e[1;38;5;#phd-ark-surface2/t256m";
fi;
