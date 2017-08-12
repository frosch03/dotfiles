# For emacs Tramp                                                                                                                                              
#################                                                                                                                                              
if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='> '
fi



#####
# $Id$
#####

loadconfig ~/.alias
loadconfig ~/.alias-zsh
loadconfig ~/.function
if [[ -d $ZSHDIR ]]; then
    loadconfig $ZSHDIR/bindkey
    loadconfig $ZSHDIR/completion
fi
watch=notme
LOGCHECK=60

hash -d doc=/usr/share/doc
#hash -d navi=~/work/navi/blub/navi

[ -f ~/.r ] && cat .r


# Add local bin dir to path, if exists
if [ -d ~/bin ] ; then
    PATH=~/bin:$PATH
fi

# Add local cabals bin dir to path, if exists
if [ -d ~/.cabal/bin ] ; then
    PATH=~/.cabal/bin:$PATH
fi

# Add local ruby gem bin dir to path, if exists
if [ -d ~/.gem/ruby/1.9.1/bin ] ; then
    PATH=~/.gem/ruby/1.9.1/bin:$PATH
fi

# COMPLETION SETTINGS
# add custom completion scripts
fpath=(~/.zsh/complete $fpath) 


# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2

# 
zstyle ":completion:*:descriptions" format "%B%d%b"



autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
# eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
# eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
eval PR_BOLD_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
eval PR_$color='%{$fg[${(L)color}]%}'

(( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"


# function setprompt {
# L_HIST="(%!)"
# L_RETVAL="%(?..%?)"
# export RPROMPT="$PR_RED $L_RETVAL ${BATSTAT} $PR_NO_COLOUR"
# if [[ $UID -eq 0 ]] ; then
#         COLOR=$PR_RED
#         USP=%m:
# else 
#         COLOR=$PR_BLUE
#         USP=%n@%m:
# fi
# export PROMPT='[${COLOR}$L_HIST$PR_NO_COLOUR] $USP%~%(!.#.$) '
# }

case $TERM in
xterm*|rxvt*)
PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~  %y\a%}'
;;
screen)
PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ %y\e\\%}'
;;
*)
PR_TITLEBAR=''
;;
esac

preexec() {
  NAME=""
  if [[ "$TERM" == screen* ]]; then
#          local CMD=${1[(wr)^(*=*|sudo|ssh|-*)]}       # dont't use hostname
      local CMD="${1[(wr)^(*=*|sudo|ssh|-*)]}$NAME" # use hostname
      echo -ne "\ek$CMD\e\\"
  fi
}
# Decide whether to set a screen title
if [[ "$TERM" == "screen" ]]; then
    PR_STITLE=$'%{\ekzsh\e\\%}'
else
    PR_STITLE=''
fi

prompt_newline=$'\n%{\r%}'
L_WHERE=$PR_CYAN"["%(!.$PR_RED.$PR_NO_COLOR)"%n@%m"$PR_CYAN"]"$PR_NO_COLOR
L_DATE_TIME=$PR_GREEN"%D{%Y-%m-%d} %*"$PR_NO_COLOR
L_PATH=$PR_BLUE"["$PR_NO_COLOR"%~"$PR_BLUE"]"$PR_NO_COLOR" "$PR_CYAN"->"$PR_NO_COLOR
L_HIST="hist"$PR_CYAN"{"$PR_YELLOW"%!"$PR_CYAN"}"$PR_NO_COLOR
L_RETVAL=%(?..$PR_CYAN"{"$PR_RED"%?"$PR_CYAN"}"$PR_NO_COLOR" ")
PROMPT=$L_WHERE" "$L_RETVAL$L_DATE_TIME"$prompt_newline"$L_PATH" $PR_STITLE ${(e)PR_TITLEBAR}"



if [[ -x ~/bin/battery.pl ]] ; then
function precmd () {
export RPROMPT="$(~/bin/battery.pl) $PR_NO_COLOR"
}
fi

setopt   notify pushdtohome autolist
setopt   longlistjobs share_history  inc_append_history 
setopt   autoresume histignoredups pushdsilent NO_BG_NICE
setopt   autopushd pushdminus extendedglob rcquotes mailwarning 


setopt RM_STAR_SILENT
setopt HIST_REDUCE_BLANKS # schoenere History ;)
setopt NO_HIST_BEEP
setopt MULTIOS
export WORDCHARS="."

setopt NO_FLOW_CONTROL


autoload -U compinit
compinit



    # auto_remove_slash    \

setopt                       \
    append_history       \
    auto_list            \
    auto_menu            \
    auto_param_keys      \
    auto_param_slash     \
    auto_pushd           \
    bad_pattern          \
    bang_hist            \
    correct              \
 NO_beep                 \
 NO_NOMATCH              \
    complete_aliases     \
    equals               \
    extended_glob        \
    extended_history     \
    function_argzero     \
    glob                 \
 NO_glob_assign          \
    glob_complete        \
 NO_glob_dots            \
    glob_subst           \
    hash_cmds            \
    hash_dirs            \
    hash_list_all        \
    hist_allow_clobber   \
    hist_beep            \
    hist_ignore_dups     \
    hist_ignore_space    \
 NO_hist_no_store        \
    hist_verify          \
 NO_hup                  \
 NO_ignore_braces        \
 NO_ignore_eof           \
    interactive_comments \
 NO_list_ambiguous       \
 NO_list_beep            \
    list_types           \
    long_list_jobs       \
    magic_equal_subst    \
 NO_mail_warning         \
 NO_mark_dirs            \
 NO_menu_complete        \
    multios              \
    numeric_glob_sort    \
 NO_overstrike           \
    path_dirs            \
    posix_builtins       \
 NO_print_exit_value     \
 NO_prompt_cr            \
    prompt_subst         \
    pushd_ignore_dups    \
 NO_pushd_minus          \
    pushd_silent         \
    pushd_to_home        \
    rc_expand_param      \
 NO_rc_quotes            \
 NO_rm_star_silent       \
 NO_sh_file_expansion    \
    sh_option_letters    \
    short_loops          \
 NO_sh_word_split        \
 NO_single_line_zle      \
 NO_sun_keyboard_hack    \
 NO_verbose              \
    zle
DIRSTACKSIZE=20
if [[ -f ~/.zdirs ]] && [[ ${#dirstack[*]} -eq 0 ]]; then
    dirstack=( ${(uf)"$(< ~/.zdirs)"} )
    # "cd -" won't work after login by just setting $OLDPWD, so                         
    cd $dirstack[0] && cd - > /dev/null
fi
chpwd() { builtin dirs -pl >! ~/.zdirs }

stty stop ""

alias ls="ls --color -a"
alias vi="vim"
alias cal="cal -m"
alias grep="grep -H --color"
alias emc="emacsclient -nw"
alias wcnbb="ssh frosch03@frosch03.de -t tmux a"

# export TERM="xterm"
#export EDITOR=vim
export EDITOR=emacs

#[[ $EMACS = t ]] && unsetopt zle

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=$PATH:$HOME/.node/bin # Add node.js to PATH

export PERL_LOCAL_LIB_ROOT="/home/frosch03/perl5";
export PERL_MB_OPT="--install_base /home/frosch03/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/frosch03/perl5";
export PERL5LIB="/home/frosch03/perl5/lib/perl5/x86_64-linux-thread-multi:/home/frosch03/perl5/lib/perl5";
export PATH="/home/frosch03/perl5/bin:$PATH";


### START-Keychain ###
# Let  re-use ssh-agent and/or gpg-agent between logins
/usr/bin/keychain $HOME/.ssh/id_rsa
source $HOME/.keychain/$(hostname)-sh
### End-Keychain ###
