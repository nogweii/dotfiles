# vcs_info
autoload -Uz vcs_info
zstyle ':vcs_info:*' stagedstr '%F{28}●' # after `git add`
zstyle ':vcs_info:*' unstagedstr '%F{11}●' # modified & tracked, file
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:[svn]' formats '[%b%c%u]'
zstyle ':vcs_info:*' enable bzr git svn hg #cvs #darcs
setopt prompt_subst

function precmd {
    # Terminal width = width - 1 (for lineup)
    local TERMWIDTH
    ((TERMWIDTH=${COLUMNS} - 1))

    # Truncate long paths
    PR_FILLBAR=""
    PR_PWDLEN=""
    local PROMPTSIZE="${#${(%):---(%n@%m)---()--}}"
    local PWDSIZE="${#${(%):-%~}}"
    if [[ "${PROMPTSIZE} + ${PWDSIZE}" -gt ${TERMWIDTH} ]]; then
        ((PR_PWDLEN=${TERMWIDTH} - ${PROMPTSIZE}))
    else
        PR_FILLBAR="\${(l.((${TERMWIDTH} - (${PROMPTSIZE} + ${PWDSIZE})))\
..${PR_HBAR}.)}"
    fi

    vcsformat=' %F{green}(%F{24}%b%c%u'
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) &&
          -z $(bzr ls --unknown 2> /dev/null) ]]; then
        # No untracked files (or they are all ignored) to worry about
        zstyle ':vcs_info:*' formats "$vcsformat%F{green})"
    else
        zstyle ':vcs_info:*' formats "$vcsformat%F{red}●%F{green})"
    fi
    vcs_info

    # Change the color of the directory if it's not writable
    if [[ ! -w $PWD ]]; then
        PR_PWD_COLOR=$PR_BRIGHT_YELLOW
    else
        PR_PWD_COLOR=$PR_BRIGHT_RED
    fi

}

function preexec () {
    # Screen window titles as currently running programs
    if [[ "${TERM}" == "screen-256color" ]]; then
        local CMD="${1[(wr)^(*=*|sudo|-*)]}"
        echo -n "\ek$CMD\e\\"
    fi
}

function setprompt () {
    if [[ "${terminfo[colors]}" -ge 8 ]]; then
        colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
        eval PR_BRIGHT_"${color}"="%{${terminfo[bold]}$fg[${(L)color}]%}"
        eval PR_"${color}"="%{$fg[${(L)color}]%}"
    done
    PR_NO_COLOUR="%{${terminfo[sgr0]}%}"

    # Try to use extended characters to look nicer
    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{${terminfo[enacs]}%}"
    PR_SHIFT_IN="%{${terminfo[smacs]}%}"
    PR_SHIFT_OUT="%{${terminfo[rmacs]}%}"
    PR_HBAR="${altchar[q]:--}"
    PR_ULCORNER="${altchar[l]:--}"
    PR_LLCORNER="${altchar[m]:--}"
    PR_LRCORNER="${altchar[j]:--}"
    PR_URCORNER="${altchar[k]:--}"

    # Terminal prompt settings
    PROMPT='$PR_SET_CHARSET$PR_GREEN$PR_SHIFT_IN$PR_ULCORNER$PR_GREEN$PR_HBAR\
$PR_SHIFT_OUT($PR_GREEN%(!.%SROOT%s.%n)$PR_WHITE@$PR_GREEN%m$PR_GREEN)\
$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_HBAR${(e)PR_FILLBAR}$PR_GREEN$PR_HBAR\
$PR_SHIFT_OUT($PR_PWD_COLOR%$PR_PWDLEN<...<%~%<<$PR_NO_COLOUR$PR_GREEN)\
$PR_SHIFT_IN$PR_HBAR$PR_GREEN$PR_URCORNER$PR_SHIFT_OUT\

$PR_GREEN$PR_SHIFT_IN$PR_LLCORNER$PR_GREEN$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_RED%?$PR_WHITE:)%(!.$PR_RED.$PR_YELLOW)%#$PR_GREEN)$PR_NO_COLOUR '

    RPROMPT='${vcs_info_msg_0_}$PR_GREEN$PR_SHIFT_IN$PR_HBAR$PR_GREEN\
$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'

    # Continuation prompt (e.g. unfinished quote, for loop...)
    PROMPT2='$PR_SET_CHARSET$PR_GREEN$PR_SHIFT_IN$PR_HBAR$PR_HBAR$PR_SHIFT_OUT(\
$PR_YELLOW%_$PR_GREEN)$PR_SHIFT_IN$PR_HBAR$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '
}

# Prompt init
setprompt
