# zshenv: Define environment variables

# Hard-coded path to the checkout of my dotfiles, derived from the zshenv
# symlink (this file).
local DOTSDIR="${${:-$HOME/.zshenv}:A:h}"

# Make sure the arrays only contain unique values
typeset -gU cdpath fpath mailpath manpath path
# And make sure these variables are exported to subcommands
typeset -x PATH MANPATH

# And now, set them
path=(
  $HOME/bin             # My own scripts, programs, etc
  $HOME/.rubygems/bin   # Binaries provided by user installed gems
  /usr/lib/cw           # Colorized versions of GNU coreutils
  /usr/{bin,sbin}       # Most programs & binaries
  /{bin,sbin}           # Lower-level, "basic" commands
  $path                 # System-provided paths
  /usr/local/{bin,sbin} # Local administrator installed/odd-ball commands
  /opt/java/bin         # Some java installations
  /usr/local/heroku/bin # Heroku toolbelt, as installed from a package
  $HOME/.cabal/bin      # User-installed haskell packages
)

fpath=(
  $DOTSDIR/config/zsh/plugins/users-completions/src/
  $fpath
)

manpath=(
  ${${:-~/.rubygems/gems/*/man/*.[0-9]}:A:h} # Gem-installed man pages
  /usr/local/share/man
  /usr/share/man
  $manpath
)

# Given a list of arrays, delete any values from each that don't point to a
# directory
function _env_trim_nonexistant_from {
  local a
  for a in "$@"; do
    integer i
    for (( i=1; i<=${(P)#a}; i++ )); do
      if [[ ! -d ${(P)${a}[i]} ]]; then
        eval "${(q)a}[${i}]=()"
      fi
    done
  done
}

# Make sure the variables only have paths that exist within
_env_trim_nonexistant_from path fpath manpath
unfunction _env_trim_nonexistant_from

# Make sure we have a language set
if ! (( $+LANG )) || [[ -z "$LANG" ]]; then
  export LC_ALL="en_US.UTF-8"
  emulate -R sh -c "$(locale)"
fi
