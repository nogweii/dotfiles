# Configure rbenv. This is similar to the output of rbenv init, though a bit
# more neckbeard-y so that I can move the root directory. (Avoiding yet another
# dotfile in $HOME)

export RBENV_ROOT="${XDG_DATA_HOME}/rbenv"

export PATH="${RBENV_ROOT}/shims:${PATH}"

source "/usr/lib/rbenv/completions/rbenv.zsh"

rbenv rehash 2>/dev/null

rbenv() {
  typeset command
  command="$1"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  rehash|shell)
    eval `rbenv "sh-$command" "$@"`;;
  *)
    command rbenv "$command" "$@";;
  esac
}

