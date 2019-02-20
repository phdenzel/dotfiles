####################################################### Bash profile
# Diagnostics
# if which brew &> /dev/null; then
#     PS4='+ $(gdate "+%s.%N")\011 '
#     exec 3>&2 2>/tmp/bashstart.$$.log
#     set -x
# else
#     PS4='+ $(date "+%s.%N")\011 '
#     exec 3>&2 2>/tmp/bashstart.$$.log
#     set -x
# fi
#######################################################
# Determine OS
if `uname -a | grep -q "Microsoft"`; then
    export OS=Microsoft
elif `uname -a | grep -q "Darwin"`; then
    export OS=Darwin
else
    export OS=Linux
fi;

####################################################### Source the dotfiles
# Note: color variables are defined in .exports; thus every command
#       using colors has to be sourced/executed thereafter
for file in ~/.{path,exports,prompt,aliases,functions}; do
    [ -r "$file" ] && [ -f "$file" ] && . "$file"
done;
unset file;


####################################################### General settings
# Append to the history file, don't overwrite it
shopt -s histappend;
# Check window size constantly
shopt -s checkwinsize
# Use globstar '**'
shopt -s globstar
shopt -s autocd
# Auto-correct typos in path names when using 'cd'
shopt -s cdspell
# Case-insensitive globbing
#shopt -s nocaseglob
# Shut up the annoying bell
if [ -f /usr/bin/setterm ]; then
    setterm blength 0
fi;

# Add tab completion for many Bash commands
if which brew &> /dev/null && [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# On macOS
# Add tab completion for defaults read/write NSGlobalDomain
# Add tab completion for killall with common apps
# Add private ssh-key to the ephemeral ssh-agent
# use -K option if using OSX Keychain and passphrase
if [ "$OS" == "Darwin" ]; then
    complete -W "NSGlobalDomain" defaults;
    complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal" killall;
    ssh-add -K	~/.ssh/id_rsa &>/dev/null
    # ssh-add ~/.ssh/id_rsa
fi;

# Reference the DISPLAY on Ubuntu on Windows
if [ "$OS" == "Microsoft" ]; then
	  export DISPLAY=localhost:0.0
fi;

# configure git
source ~/.USERINFO
git config --global user.name "$GITUSER"
git config --global user.email "$GITMAIL"

####################################################### Welcome helpers
# Get some informations about the system
_hostinfo () {
    echo -e "${orange}Host: \t\t\t\t ${blue}${HOSTNAME}"
    echo -e "${orange}Operating System: \t\t${blue} \c"
    case $OS in
        Darwin)
            echo $(sw_vers | awk -F ':' '{print $2}')
            ;;
        Linux)
            echo "$(cat /etc/issue | sed 's/Welcome to //g' | sed 's/\\[a-z]//g' | sed 's/(\\l)\.//g')"
            ;;
        Microsoft)
            echo "$(cat /etc/issue | sed 's/\\n//g' | sed 's/\\l//g')"
            ;;
        *)
            echo "$(cat /etc/issue)"
            ;;
    esac;
    echo -e  "${orange}Kernel Information: \t\t${blue}" `uname -smr`
    echo -ne "${orange}Hello ${magenta}$USER${orange} today is: \t${blue}" `date`;
    echo -e "${reset}";
}
# Get the mounted drives and free space
_mounted () {
    echo "-------------------------------- Mounted Drives --------------------------------"
    case $OS in
        Microsoft)
            df -H
            ;;
	      Darwin)
	          gdf -H
	          ;;
        *)
            df -H
            ;;
    esac
}
# Check free memory ((uses $OS set in .aliases))
_meminfo () {
    echo "------------------------------ Memory Information ------------------------------"
    case $OS in
        Microsoft)
            /usr/bin/free -tm
            ;;
        Darwin)
            free --megabyte
            ;;
	      *)
	          /usr/bin/free -tm
            ;;
    esac
}
# Look at uptime
_upinfo () {
    echo "------------------------------------ Uptime ------------------------------------"
    echo ""
    echo -ne "Uptime for: ${HOSTNAME} is "; uptime | awk /'up/ {print $3,$4,$5,$6,$7,$8,$9,$10}'
}


####################################################### Welcome Screen
clear
echo -e "${magenta}+++++++++++++++++++++++++++++++++${reset}${bold} W E L C O M E ${reset}${magenta}++++++++++++++++++++++++++++++++"; echo ""

if [ -f "local/bin/neofetch" ]; then
    neofetch
else
    _hostinfo
fi;
cal -3
echo -ne "${green}"; _mounted; echo -ne "${reset}"
echo -ne "${orange}"; _meminfo; echo -ne "${reset}"
# echo -ne "${blue}"; _upinfo;  echo "${reset}"
echo "";
echo -e  "${blue}${HOSTNAME}${reset} at your service";

unset _hostinfo;
unset _mounted;
unset _meminfo;
unset _upinfo;
#######################################################
# Turn of diagnostics
# set +x
# exec 2>&3 3>&-
####################################################### END bash profile
