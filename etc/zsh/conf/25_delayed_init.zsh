typeset -ga _lazy_compdef
typeset -ga delayed_init_functions

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

  # Set up zsh's completion engine, using a cache
  compsupercache

  # Replay compdef calls.
  local args
  for args in $_lazy_compdef; do
    compdef "${(@0)args}"
  done
  unset _lazy_compdef

  # Do any set up that has been delayed
  local func
  for func in $delayed_init_functions; do
    $func
  done
  unset delayed_init_functions
}

precmd_functions=(_delayed_oneoff_init $precmd_functions)
