####################################################### bashrc
# Diagnostics
# if command -v brew &> /dev/null; then
#     PS4='+ $(gdate "+%s.%N")\011 '
#     exec 3>&2 2>/tmp/bashstart.$$.log
#     set -x
# else
#     PS4='+ $(date "+%s.%N")\011 '
#     exec 3>&2 2>/tmp/bashstart.$$.log
#     set -x
# fi
#######################################################

if [ -f ${XDG_CONFIG_HOME:=$HOME/.config}/bashrc/bashrc ]; then
    . ${XDG_CONFIG_HOME:=$HOME/.config}/bashrc/bashrc
fi;

#######################################################
# Turn of diagnostics
# set +x
# exec 2>&3 3>&-
####################################################### END bash profile
