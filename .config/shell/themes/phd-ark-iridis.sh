### PhD Ark Iridis color palette
# Note: These variables are not exported! The color variables are restricted to
#       the current shell only, and have to be sourced with each new shell session.
if tput setaf 1 &> /dev/null; then
    tput sgr0; # reset
    bold=$(tput bold);
    underline=$(tput smul);
    underline_reset=$(tput rmul);
    reset=$(tput sgr0);
    # Custom color palette (see .dircolors)
    crust=$(tput setaf );
    red=$(tput setaf 1);
    green=$(tput setaf 2);
    yellow=$(tput setaf 3);
    blue=$(tput setaf 4);
    pink=$(tput setaf 5);
    teal=$(tput setaf 6);
    subtext0=$(tput setaf 7);
    surface1=$(tput setaf 8);
    ruby=$(tput setaf 9);
    viridis=$(tput setaf 10);
    sand=$(tput setaf 11);
    indigo=$(tput setaf 12);
    magenta=$(tput setaf 13);
    cyan=$(tput setaf 14);
    white=$(tput setaf 15);

    crimson=$(tput setaf 160);
    tiger=$(tput setaf 202);
    emerald=$(tput setaf 35);
    lilac=$(tput setaf 99);
    purple=$(tput setaf 105);

    orange=$(tput setaf 214);
    grass=$(tput setaf 72);
    ocean=$(tput setaf 25);
    turquoise=$(tput setaf 43);
    amethyst=$(tput setaf 57);
    violet=$(tput setaf 147);
    surface0=$(tput setaf 237);
    subtext1=$(tput setaf 253);
    textcolor=$(tput setaf 251);
    overlay2=$(tput setaf 245);
    surface2=$(tput setaf 103);
else
    crust="\e[1;38;5;000m";
    red="\e[1;38;5;001m";
    green="\e[1;38;5;002m";
    yellow="\e[1;38;5;003m";
    blue="\e[1;38;5;004m";
    pink="\e[1;38;5;005m";
    teal="\e[1;38;5;006m";
    subtext0="\e[1;38;5;007m";
    surface1="\e[1;38;5;008m";
    ruby="\e[1;38;5;009m";
    viridis="\e[1;38;5;010m";
    sand="\e[1;38;5;011m";
    indigo="\e[1;38;5;012m";
    magenta="\e[1;38;5;013m";
    cyan="\e[1;38;5;014m";
    white="\e[1;38;5;015m";

    crimson="\e[1;38;5;160m";
    tiger="\e[1;38;5;202m";
    emerald="\e[1;38;5;035m";
    lilac="\e[1;38;5;099m";
    purple="\e[1;38;5;105m";

    orange="\e[1;38;5;214m";
    grass="\e[1;38;5;072m";
    ocean="\e[1;38;5;025m";
    turquoise="\e[1;38;5;043m";
    amethyst="\e[1;38;5;057m";
    violet="\e[1;38;5;147m";
    surface0="\e[1;38;5;237m";
    subtext1="\e[1;38;5;253m";
    textcolor="\e[1;38;5;251m";
    overlay2="\e[1;38;5;245m";
    surface2="\e[1;38;5;103m";
fi;
