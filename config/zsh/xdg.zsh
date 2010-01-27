# XDG-related stuff

export XDG_CACHE_HOME="${HOME}/.cache/"
if [ ! -d $XDG_CACHE_HOME ] ; then
    mkdir -p $XDG_CACHE_HOME
fi

export XDG_CONFIG_HOME="${HOME}/.config/"
if [ ! -d $XDG_CONFIG_HOME ] ; then
    mkdir -p $XDG_CONFIG_HOME
fi

export XDG_DATA_HOME="${HOME}/.data/"
if [ ! -d $XDG_DATA_HOME ] ; then
    mkdir -p $XDG_DATA_HOME
fi

# Other directories (colon separated) to search through.
export XDG_DATA_DIRS="${HOME}/.data/:${HOME}/.local/share/:${XDG_DATA_DIRS}"
export XDG_CONFIG_DIRS="${HOME}/.config/:${HOME}/.local/config/:${XDG_CONFIG_DIRS}"
