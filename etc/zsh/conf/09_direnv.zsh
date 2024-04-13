if [ -n "${commands[direnv]}" ]; then
  _direnv_hook() {
    if [ -z "$_disable_direnv" ]; then
      return 0
    else
      unset _disable_direnv
    fi
    eval "$("${commands[direnv]}" export zsh)";
  }

  typeset -ag precmd_functions;
  if [[ -z "${precmd_functions[(r)_direnv_hook]+1}" ]]; then
    precmd_functions=( _direnv_hook ${precmd_functions[@]} )
  fi

  typeset -ag chpwd_functions;
  if [[ -z "${chpwd_functions[(r)_direnv_hook]+1}" ]]; then
    chpwd_functions=( _direnv_hook ${chpwd_functions[@]} )
  fi
fi
