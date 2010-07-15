# Prompt. Make it purrty!
export PROMPT=$'%B%n%b%d%(?..[%?])/\n> '
export RPROMPT="-- INSERT --"
#DEFPROMPT='${BLUE}%~ ${PROMPTCOLOR}${PROMPTCHAR}${NC} '   # promptcolor/promptchar match hostname coloring and show a different char if ssh'd somewhere
#DEFRPROMPT=$SSHPROMPT     # shows @ when ssh'd somewhere else

autoload -Uz vcs_info
#zstyle ':vcs_info:*' enable git                                 # only enable git/git-svn
zstyle ':vcs_info:*:prompt:*' check-for-changes true
zstyle ':vcs_info:*:prompt:*' unstagedstr "*"
zstyle ':vcs_info:*:prompt:*' stagedstr "+"
zstyle ':vcs_info:*:prompt:*' branchformat "%b"
zstyle ':vcs_info:*:prompt:*' actionformats "[%b%c%u:%a]"
zstyle ':vcs_info:*:prompt:*' formats "[%b%c%u]"
zstyle ':vcs_info:*:prompt:*' nvcsformats "no"

function precmd() {
    vcs_info 'prompt'
    if [[ $vcs_info_msg_0_ != "no" ]]; then
        RPROMPT="${vcs_info_msg_0_}"
    else
        RPROMPT=$DEFRPROMPT
    fi
}
