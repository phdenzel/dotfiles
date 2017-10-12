####################################################### Bash profile
# Source the dotfiles
# Note: color variables are defined in .exports; thus every command
#       using colors has to be sourced/executed thereafter
for file in ~/.{path,exports,prompt,aliases,functions}; do
    [ -r "$file" ] && [ -f "$file" ] && . "$file"
done;
unset file;

####################################################### General settings
# Add private ssh-key to the ephemeral ssh-agent
# use -K option if using OSX Keychain and passphrase
ssh-add -K	~/.ssh/id_rsa &>/dev/null
#ssh-add ~/.ssh/id_rsa
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
    setterm -blength 0
fi;

# Add tab completion for many Bash commands
if which brew &> /dev/null && [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
# Add tab completion for defaults read/write NSGlobalDomain
complete -W "NSGlobalDomain" defaults;
# Add tab completion for killall with common apps
complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal" killall;

# configure git
source ~/.USERINFO
git config --global user.name "$GITUSER"
git config --global user.email "$GITMAIL"

####################################################### Welcome helpers
# Get the mounted drives and free space
_mounted () {
    echo "-------------------------------- Mounted Drives --------------------------------"
    df -h
}
# Check free memory
_meminfo () {
    echo "------------------------------ Memory Information ------------------------------"
    case $( uname -s ) in
	Linux)
	    /usr/bin/free -tm
	;;
	*)
	    free --megabyte
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
echo -e  "${magenta}+++++++++++++++++++++++++++++++++${reset}${bold} W E L C O M E ${reset}${magenta}++++++++++++++++++++++++++++++++"; echo ""
echo -e  "${orange}Host: \t\t\t\t ${blue}${HOSTNAME}"
if type sw_vers >/dev/null 2>&1; then
    echo -e  "${orange}Operating System: \t\t${blue}"\
         $(sw_vers | awk -F ':' '{print $2}');
else
    echo -e  "${yellow}Operating System: \t\t${blue}" \
    `cat /etc/issue | cut -c 1-19`;
    # cat /etc/issue | awk 'BEGIN { ORS=" " } \
    # FNR==1 {print $0}'; # FNR==2 {print $1}';
    echo "";    
fi;
echo -e  "${orange}Kernel Information: \t\t${blue}" `uname -smr`
echo -ne "${orange}Hello ${magenta}$USER${orange} today is: \t${blue}" `date`;
echo -e "${reset}";
if type gcal >/dev/null 2>&1 ; then
    gcal . | awk 'NR>4 {print}'
elif type cal >/dev/null 2>&1 ; then
    echo ""; cal -3
fi;
echo -ne "${green}"; _mounted; echo "${reset}"
echo -ne "${orange}"; _meminfo; echo "${reset}"
echo -ne "${blue}"; _upinfo;  echo "${reset}"
echo -e  "${blue}${HOSTNAME}${reset} at your service"; echo ""

unset _mounted;
unset _meminfo;
unset _upinfo;
