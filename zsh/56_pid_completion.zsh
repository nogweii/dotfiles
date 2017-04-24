function pid-funcs-completion() {
  local tokens cmd fzf matches
  setopt localoptions noshwordsplit noksh_arrays noposixbuiltins

  # http://zsh.sourceforge.net/FAQ/zshfaq03.html
  # http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion-Flags
  tokens=(${(z)LBUFFER})
  if [ ${#tokens} -lt 1 ]; then
    zle expand-or-complete
    return
  fi

  cmd=${tokens[1]}

  # Process search, made all fancy with fzf.
  if [[ ( $cmd = "pidenv" || $cmd = "pidstarted" ) && ${LBUFFER[-1]} = ' ' ]]; then
    fzf="$(__fzfcmd_complete)" # decide on fzf vs fzf-tmux

    # show the fzf window to the user
    matches=$(ps -ef | sed 1d | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-50%} --min-height 15 --reverse $FZF_DEFAULT_OPTS --preview 'echo {}' --preview-window down:3:wrap $FZF_COMPLETION_OPTS" ${=fzf} -m | awk '{print $2}' | tr '\n' ' ')

    # append what they selected, if anything
    if [ -n "$matches" ]; then
      LBUFFER="$LBUFFER$matches"
    fi

    # and redraw the display
    zle redisplay
    typeset -f zle-line-init >/dev/null && zle zle-line-init
  elif [[ $cmd = "pidof" && ${LBUFFER[-1]} = ' ' ]]; then
    # This is all the same as above except now we print out the process' name, not ID, from awk
    fzf="$(__fzfcmd_complete)"
    matches=$(ps -ef | sed 1d | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-50%} --min-height 15 --reverse $FZF_DEFAULT_OPTS --preview 'echo {}' --preview-window down:3:wrap $FZF_COMPLETION_OPTS" ${=fzf} -m | awk '{print $8}' | tr '\n' ' ')
    if [ -n "$matches" ]; then
      LBUFFER="$LBUFFER$matches"
    fi
    zle redisplay
    typeset -f zle-line-init >/dev/null && zle zle-line-init
  else
    # Fall back to zsh's own completion function
    zle expand-or-complete
  fi
}

# Only enable the fancy fzf completion when it's installed
if [ -n "${commands[fzf]}" ]; then
  zle     -N   pid-funcs-completion
  bindkey '^I' pid-funcs-completion
  # NB: This overrides the standard zsh completion keybinding. That's why it
  # will fall back to the default, expand-or-complete. fzf's own magic
  # keybind setup will detect the presence of this function and properly fallback
fi
