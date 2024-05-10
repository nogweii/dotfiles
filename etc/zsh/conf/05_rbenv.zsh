# Configure rbenv. This is similar to the output of rbenv init, though a bit
# more neckbeard-y so that I can move the root directory. (Avoiding yet another
# dotfile in $HOME)

# Early return to skip sourcing the rest of the file if rbenv isn't installed
if [[ -z $commands[rbenv] ]]; then
   return
fi

export RBENV_SHELL=zsh

# Get the directory rbenv is in (e.g. installed to /usr/local/rbenv/? The full
# path to the rbenv command is /usr/local/rbenv/bin/rbenv and I want just just
# '/usr/local/rbenv')
local rbenv_parent_path=${${:-$commands[rbenv]:A/../../}:A}

# If the parent path is writable by me, use that. (For when rbenv is installed
# in my home directory)
if [[ -w $rbenv_parent_path ]]; then
  export RBENV_ROOT="${rbenv_parent_path}"
else
  # Failing that, use $XDG_DATA_HOME
  export RBENV_ROOT="${XDG_DATA_HOME}/rbenv"
fi

path=("${RBENV_ROOT}/shims" $path)

_rbenv_plugins=${${:-${RBENV_ROOT}/../rbenv-plugins}:A}
if [ -d "${_rbenv_plugins}" ]; then
  for plugin_bin in "${_rbenv_plugins}"/*/bin; do
    PATH="${PATH}:${plugin_bin}"
  done
  for plugin_hook in "${_rbenv_plugins}"/*/etc/rbenv.d; do
    RBENV_HOOK_PATH="${RBENV_HOOK_PATH}:${plugin_hook}"
  done
  typeset -x RBENV_HOOK_PATH
fi

# The completions are shipped in the same parent directory as the command
source "${rbenv_parent_path}/completions/rbenv.zsh"

rbenv rehash 2>/dev/null

rbenv() {
  local rbenv_subcommand
  rbenv_subcommand="$1"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$rbenv_subcommand" in
  rehash|shell)
    eval `rbenv "sh-$rbenv_subcommand" "$@"`;;
  *)
    command rbenv "$rbenv_subcommand" "$@";;
  esac
}

# Don't need this variable any more
unset rbenv_parent_path
