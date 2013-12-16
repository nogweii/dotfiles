# Configure rbenv. This is similar to the output of rbenv init, though a bit
# more neckbeard-y so that I can move the root directory. (Avoiding yet another
# dotfile in $HOME)

# Early return to skip sourcing the rest of the file if rbenv isn't installed
if [[ -z $commands[rbenv] ]]; then
   return 
fi

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
  rehash|shell|use)
    eval `rbenv "sh-$command" "$@"`;;
  *)
    command rbenv "$command" "$@";;
  esac

  # Update the version string for the prompt every time I call rbenv (i.e. rbenv
  # use jruby will be reflected quickly)
  _update_ruby_version
}

# A prompt function to set $ruby_version, a prompt-friendly string of the
# currently targeted rbenv ruby.
function _update_ruby_version()
{
    typeset -g ruby_version=''
    if which rbenv &> /dev/null; then
      # Strip the specific version information (i.e. jruby-1.7.4 becomes just
      # 'jruby')
      ruby_version="${$(command rbenv version-name)%%-*}"

      # If we're using the system ruby, then don't report anything
      if [ "${ruby_version}" = "system" ]; then
        ruby_version=""
      fi
    fi
}
# Update the version string every time I change directories
chpwd_functions+=(_update_ruby_version)

# And initialize $ruby_version 
_update_ruby_version
