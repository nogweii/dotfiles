# These various environment variables move stuff from out of the home directory
# and into various subdirectories within. `ls -a` should not report a million
# items, much less now!

export GEMRC="${DOTSDIR}/config/gemrc"
export BUNDLE_CONFIG="${DOTSDIR}/config/bundle/config"
export npm_config_prefix=$HOME/.local/node
export npm_config_userconfig="${DOTSDIR}/config/npmrc"
export NODE_REPL_HISTORY="${XDG_CACHE_HOME}/node_repl_history"
export BABEL_CACHE_PATH="${XDG_CACHE_HOME}/babel_cache.json"
export TRAVIS_CONFIG_PATH="${HOME}/.config/travis"
export DOCKER_CONFIG="${HOME}/.config/docker"

export LESSHISTFILE="${XDG_CACHE_HOME}/lesshist"
export HISTFILE="${HOME}/.local/cache/zsh/history"
export INPUTRC="${DOTSDIR}/config/inputrc"

export ANSIBLE_LOCAL_TEMP="${XDG_CACHE_HOME}/ansible/temp"
export ANSIBLE_SSH_CONTROL_PATH_DIR="${XDG_CACHE_HOME}/ansible/cp"

export GIMP2_DIRECTORY="${XDG_CONFIG_HOME}/gimp"

export PYTHON_EGG_CACHE="${XDG_CACHE_HOME}/python-eggs"

export GOPATH=~/.local/go

export TMUX_TMPDIR="${XDG_RUNTIME_DIR}"
[ -z "${XAUTHORITY}" ] && export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"

export RANDFILE="${XDG_CACHE_HOME}/rnd"

export MYSQL_HISTFILE="${XDG_CACHE_HOME}/mysql_history"
export WGETRC="${DOTSDIR}/config/wgetrc"
export VAGRANT_HOME="${XDG_DATA_HOME}/vagrant"

# Move the kerberos credentials cache out of /tmp
export KRB5CCNAME="${XDG_DATA_HOME}/krb5cc"
export KRB5_CONFIG="${XDG_CONFIG_HOME}/krb5_ipa.conf"

export CARGO_HOME="${HOME}/.local/cargo"

export TF_CLI_CONFIG_FILE="${XDG_CONFIG_HOME}/terraform.rc"

# Mix, for backwards compatibility, uses ~/.mix; setting this will switch to
# prioritizing the XDG settings
export MIX_XDG=please
# for every other elixir tool that doesn't know about $MIX_XDG
export MIX_HOME="${XDG_DATA_HOME}/mix"

export PSQLRC="${XDG_CONFIG_HOME}/psqlrc"
export PSQL_HISTORY="${XDG_CACHE_HOME}/psql_history"

export PYLINTRC="${XDG_CONFIG_HOME}/pylint.rc.toml"
export KREW_ROOT="${XDG_DATA_HOME}/krew"

# point ripgrep at the config file in the dotfiles clone
export RIPGREP_CONFIG_PATH="${DOTSDIR}/config/ripgreprc"

# Put everything pipx installs under ~/.local/share/pipx/
export PIPX_HOME="${XDG_DATA_HOME}/pipx"
export PIPX_BIN_DIR="${XDG_DATA_HOME}/pipx/bin"

AUTOSWITCH_VIRTUAL_ENV_DIR="${XDG_DATA_HOME}/autoswitch-venvs/"

export ATUIN_CONFIG_DIR="${DOTSDIR}/config/atuin"
