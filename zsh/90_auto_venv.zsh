# Automatically activate a python virtualenv when I enter a directory that has
# a `venv` folder (my common pattern). Automatically deactivate that
# environment when I leave the project's folder.

# Set `pip install` to install to the user directory rather than system-wide
export PIP_USER=yes

function _auto_py_venv() {
  # if I cd into a directory with the activate script
  if [ -f venv/bin/activate ]; then
    VIRTUAL_ENV_DISABLE_PROMPT=1
    unset PIP_USER
    source venv/bin/activate
  # if I cd into a directory that's not a subdirectory of the venv parent path
  elif [ -n "${VIRTUAL_ENV}" -a ! $(is-path-subdirectory "${PWD}" "${VIRTUAL_ENV:h}") ]; then
    deactivate
    export PIP_USER=yes
    unset VIRTUAL_ENV_DISABLE_PROMPT
  fi
}

add-zsh-hook chpwd _auto_py_venv
