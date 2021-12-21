# zshenv: Define environment variables

# Hard-coded path to the checkout of my dotfiles, derived from the zshenv
# symlink (this file).
local DOTSDIR="${${:-$HOME/.zshenv}:A:h}"

if [[ "${PROFILE_STARTUP:-false}" == true ]]; then
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    local startlog_file=$HOME/tmp/startlog.$$
    exec 3>&2 2>$startlog_file
    echo "xtrace output being sent to ${startlog_file} for profiling..."
    setopt xtrace prompt_subst
    zmodload zsh/zprof
fi

unsetopt NO_MATCH # Don't error on failed matches
unsetopt GLOBAL_RCS # Don't use the global zsh configs (/etc/zsh*)

# Pull in settings from /etc/profile, like the path to java, etc.
emulate sh -c 'source /etc/profile'
emulate sh -c "source ${DOTSDIR}/profile"

# And now, set them
path=(
  $HOME/bin                    # My own scripts, programs, etc
  $HOME/.local/bin             # Machine-local artisinal scripts
  $HOME/.local/ruby/bin        # User-installed ruby gems
  "${VIRTUAL_ENV:-/dev/null}/bin" # Python virtualenv
  $HOME/.local/pypi/bin        # User-installed python packages
  $HOME/.local/node/bin        # User-installed npm packages
  $HOME/.local/go/bin          # User-installed go packages
  $HOME/.local/cargo/bin       # User-installed rust packages
  /usr/local/{bin,sbin}        # Local administrator installed/odd-ball commands
  /usr/{bin,sbin}              # Most programs & binaries
  /{bin,sbin}                  # Lower-level, "basic" commands
  $path                        # System-provided paths

  $HOME/.local/share/cabal/bin # User-installed haskell packages
  $HOME/.cabal/bin
  $HOME/.heroku/client/bin     # Heroku toolbelt, via `heroku update`
  $DOTSDIR/share/rbenv/bin     # rbenv install
  $HOME/.local/elixir/bin      # Manually installed elixir 1.0 binaries
  $HOME/go/bin                 # go packages
  /usr/lib/cw                  # Colorized versions of GNU coreutils
  /usr/local/heroku/bin        # Heroku toolbelt, as installed from a package
  /opt/java/bin                # Some java installations
)

fpath=(
  $DOTSDIR/zsh/plugins/users-completions/src
  $DOTSDIR/zsh/functions
  /usr/share/zsh/site-functions
  /opt/homebrew/share/zsh/site-functions
  $fpath
)

manpath=(
  #${${:-~/.rubygems/gems/*/man/*.[0-9]}:A:h} # Gem-installed man pages
  /usr/local/share/man
  /usr/share/man
  $HOME/.local/ruby/share/man/  # Gem-installed man pages
  $manpath
)

if [ -f "/System/Library/CoreServices/SystemVersion.plist" ]; then
  # On MacOS systems, prepend Homebrew & the GNU userland to path
  path=(
    /opt/homebrew/opt/gnu-tar/libexec/gnubin
    /opt/homebrew/opt/gnu-sed/libexec/gnubin
    /opt/homebrew/opt/coreutils/libexec/gnubin
    /opt/homebrew/opt/ruby/bin
    /opt/homebrew/opt/go/libexec/bin
    /opt/homebrew/{bin,sbin}
    $path
  )
  # And get the brew installed man pages
  manpath=(
    /opt/homebrew/opt/coreutils/libexec/gnuman
    /opt/homebrew/opt/gnu-sed/libexec/gnuman
    /opt/homebrew/share/man
    $manpath
  )

  if [ -d /opt/homebrew ]; then
    export HOMEBREW_PREFIX="/opt/homebrew";
    export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
    export HOMEBREW_REPOSITORY="/opt/homebrew";
  fi

  if [ -d /opt/homebrew/opt/ruby/ ]; then
    export LDFLAGS="-L/opt/homebrew/opt/ruby/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/ruby/include"
  fi
fi

# Make sure the arrays only contain unique values
typeset -gU cdpath fpath mailpath manpath infopath path
# And make sure these variables are exported to subcommands
typeset -x PATH MANPATH INFOPATH

# Given a list of arrays, delete any values from each that don't point to a
# real directory. It will also resolve any symlinks.
function _strip_fakes {
  local a
  local check_dir
  local replacement_arr
  for a in "$@"; do
    [[ -f ${TMPDIR}/zsh_path_debug ]] && echo "${(q)a} before strip_fakes: ${(P)a}"
    replacement_arr=()
    integer i
    for (( i=1; i<=${(P)#a}; i++ )); do
      check_dir=${(P)${a}[i]}

      if [[ ! -d ${check_dir} ]]; then
        # It's not a directory! Skip it
        continue
      elif [[ -h ${check_dir} ]]; then
        # It's a symlink; resolve it
        replacement_arr+=${check_dir:A}
      elif [[ ${check_dir} =~ '/$' ]]; then
        # The given path ends in trailing slash. Homebrew warns about it, and
        # is generally not advised anyways. Strip that character.
        replacement_arr+=${check_dir[0,-2]}
      elif [[ ${check_dir} =~ '^/mnt/c/' ]]; then
        # Skip windows paths, for when I'm in LXSS.
        continue
      else
        # Vanilla folder, add it
        replacement_arr+=${check_dir}
      fi

      # Now that we've done a bunch of tweaks to the final array, make sure all
      # the values are unique, again.
      typeset -gU "${(q)a}"
    done

    [[ -f ${TMPDIR}/zsh_path_debug ]] && echo "${(q)a} after  strip_fakes: ${replacement_arr}"
    # Set the array variable to it's filtered value
    set -A "${(q)a}" $replacement_arr
  done
}
_strip_fakes path fpath manpath
unfunction _strip_fakes

# Make sure we have a language set
if [[ -z "$LANG" ]]; then
  export LC_ALL="en_US.UTF-8"
  export LANG="en_US.UTF-8"
fi

# Tell SSH about gpg-agent's SSH socket
[[ -S ~/.gnupg/S.gpg-agent.ssh ]] && export SSH_AUTH_SOCK=~/.gnupg/S.gpg-agent.ssh

# Move the user base directory to a subdirectory beneath .local, so the bin,
# lib, etc directories don't pollute the top level directory
export PYTHONUSERBASE=~/.local/pypi

export GEM_SPEC_CACHE="${HOME}/.local/cache/gem"
export GEM_HOME="${HOME}/.local/ruby"

export CARGO_HOME="${HOME}/.local/cargo"

unset PIP_USER
