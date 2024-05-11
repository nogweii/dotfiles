typeset -ga _lazy_compdef

# Define a compdef function immediately, that just queues up a bunch of calls
function compdef() {
  eval "$_z4h_opt"
  _lazy_compdef+=("${(pj:\0:)@}")
}

# This function is called on the first precmd, doing a bunch of zle changes
# later on instead of blocking zsh startup
function _delayed_oneoff_init() {
  # remove this function from the list, it should only run once
  precmd_functions=(${precmd_functions:#_delayed_oneoff_init})

  compsupercache

  # Replay compdef calls.
  local args
  for args in $_lazy_compdef; do
    compdef "${(@0)args}"
  done
  unset _lazy_compdef

  # Enable syntax highlighting:
  source ${ZDOTDIR}/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
  FAST_HIGHLIGHT_STYLES[path]=underline
}

precmd_functions=(_delayed_oneoff_init $precmd_functions)
