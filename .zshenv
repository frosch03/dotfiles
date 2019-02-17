export PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin:/usr/games
export LC_CTYPE=en_US.utf8
export EDITOR=~/bin/edit

# ARCH LINUX; is overwritten by /etc/zsh/zprofile
#if [ -d ~/.cabal/bin ] ; then
#    PATH=~/.cabal/bin:$PATH
#fi
#if [ -d ~/bin ] ; then
#    PATH=~/bin:$PATH
#fi
######
typeset -U PATH

#eval `dircolors -b ~/.dircolors`
alias ls='ls --color=auto'
alias centerim='centerim -a'

HISTFILE=~/.zshhistory
HISTSIZE=3000
SAVEHIST=3000


export ZSHDIR=$HOME/.zsh
export BROWSER=w3m

function loadconfig() {
    if [[ -f $@ ]] ; then
    
        if [[ ! -e $@.zwc ]] ; then
            zcompile $@
        fi  

        if [[ $@ -nt $@.zwc ]] ; then
            zcompile $@  
        fi  
        . $@
    fi  
}

    

export DATE=`date +%d-%m-%y`
#if [ -x /usr/bin/keychain ] ; then
#    keychain -q -Q --noask ~/.privat/identity-privat ~/.privat/identity-work
#fi

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

export PATH=$PATH:~/bin
