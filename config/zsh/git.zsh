# Most of this is from Bart Trojanowski. Thanks a lot!
# check-for-changes can be really slow in large repositories
zstyle ':vcs_info:*:prompt:*' check-for-changes true
zstyle ':vcs_info:*:prompt:*' unstagedstr   '¹'  # display ¹ if there are unstaged changes
zstyle ':vcs_info:*:prompt:*' stagedstr     '²'  # display ² if there are staged changes
zstyle ':vcs_info:*:prompt:*' actionformats "${FMT_BRANCH}${FMT_ACTION}//" "${FMT_PATH}"
zstyle ':vcs_info:*:prompt:*' formats       "${FMT_BRANCH}//"              "${FMT_PATH}"
zstyle ':vcs_info:*:prompt:*' nvcsformats   ""                             "%~"

# update the vcs_info_msg_ magic variables, but only as little as possible

# This variable dictates weather we are going to do the vcs prompt update
# before printing the next prompt.  On some setups this saves 10s of work.
PR_VCS_UPDATE=1

# called before command execution
# here we decide if we should update the prompt next time
function zsh_vcs_prompt_preexec {
        case "$(history $HISTCMD)" in 
            *vcs*)
                PR_VCS_UPDATE=1
                ;;
        esac
}
preexec_functions+='zsh_vcs_prompt_preexec'

# called after directory change
# we just assume that we have to update vcs prompt
function zsh_vcs_prompt_chpwd {
        PR_VCS_UPDATE=1
}
chpwd_functions+='zsh_vcs_prompt_chpwd'

# called before prompt generation
# if needed, we will update the prompt info
function zsh_vcs_prompt_precmd {
       if [[ -n "$PR_VCS_UPDATE" ]] ; then
               vcs_info 'prompt'
               PR_VCS_UPDATE=
       fi
}
precmd_functions+='zsh_vcs_prompt_precmd'
