# zshenv: Define environment variables

# Hard-coded path to the checkout of my dotfiles, derived from the zshenv
# symlink (this file).
local DOTSDIR="${${:-$HOME/.zshenv}:A:h}"

unsetopt NO_MATCH # Don't error on failed matches
unsetopt GLOBAL_RCS # Don't use the global zsh configs (/etc/zsh*)

# Pull in settings from /etc/profile, like the path to java, etc.
emulate sh -c 'source /etc/profile'
emulate sh -c "source ${DOTSDIR}/profile"

# And now, set them
path=(
  $HOME/bin                    # My own scripts, programs, etc
  $HOME/.local/ruby/bin        # User-installed rubygems
  $HOME/.local/pypi/bin        # User-installed pip packages
  $HOME/.local/node/bin        # User-installed npm packages
  $HOME/.local/go/bin          # User-installed go packages
  /usr/local/{bin,sbin}        # Local administrator installed/odd-ball commands
  /usr/{bin,sbin}              # Most programs & binaries
  /{bin,sbin}                  # Lower-level, "basic" commands
  $path                        # System-provided paths

  $HOME/.local/share/cabal/bin # User-installed haskell packages
  $HOME/.cabal/bin
  $HOME/.heroku/client/bin     # Heroku toolbelt, via `heroku update`
  $DOTSDIR/share/rbenv/bin     # rbenv install
  $HOME/.local/bin             # User-installed Python packages
  $HOME/.local/elixir/bin      # Manually installed elixir 1.0 binaries
  $HOME/.gem/ruby/*/bin(On)    # rubygems installed, newest first
  $HOME/go/bin                 # go packages
  /usr/lib/cw                  # Colorized versions of GNU coreutils
  /usr/local/heroku/bin        # Heroku toolbelt, as installed from a package
  /opt/java/bin                # Some java installations
)

fpath=(
  $DOTSDIR/zsh/plugins/users-completions/src
  $DOTSDIR/zsh/functions
  /usr/share/zsh/site-functions
  $fpath
)

manpath=(
  #${${:-~/.rubygems/gems/*/man/*.[0-9]}:A:h} # Gem-installed man pages
  /usr/local/share/man
  /usr/share/man
  $manpath
)

if [ -n "${IS_OSX}" ]; then
  # On OSX systems, prepend the GNU userland to path
  path=(
    /usr/local/opt/gnu-tar/libexec/gnubin
    /usr/local/opt/gnu-sed/libexec/gnubin
    /usr/local/opt/coreutils/libexec/gnubin
    $path
    /usr/local/opt/go/libexec/bin
  )
  # And get the GNU man pages, not the OSX ones
  manpath=(
    /usr/local/opt/coreutils/libexec/gnuman
    /usr/local/opt/gnu-sed/libexec/gnuman
    $manpath
  )
fi

# Make sure the arrays only contain unique values
typeset -gU cdpath fpath mailpath manpath path
# And make sure these variables are exported to subcommands
typeset -x PATH MANPATH

# Given a list of arrays, delete any values from each that don't point to a
# real directory. It will also resolve any symlinks.
function _strip_fakes {
  local a
  local check_dir
  local replacement_arr
  for a in "$@"; do
    replacement_arr=()
    integer i
    for (( i=1; i<=${(P)#a}; i++ )); do
      check_dir=${(P)${a}[i]}

      if [[ ! -d ${check_dir} ]]; then
        eval "${(q)a}[${i}]=()"
        continue
      elif [[ -h ${check_dir} ]]; then
        eval "${(q)a}[${i}]=(${(P)${a}[i]:A})"
        replacement_arr+=${check_dir:A}
      else
        replacement_arr+=${check_dir}
      fi
    done

    set -A "${(q)a}" $replacement_arr
  done
}
_strip_fakes path fpath manpath
unfunction _strip_fakes

# Make sure we have a language set
if ! (( $+LANG )) || [[ -z "$LANG" ]]; then
  export LC_ALL="en_US.UTF-8"
  emulate -R sh -c "$(locale)"
fi

# Tell SSH about gpg-agent's SSH socket
[[ -S ~/.gnupg/S.gpg-agent.ssh ]] && export SSH_AUTH_SOCK=~/.gnupg/S.gpg-agent.ssh
