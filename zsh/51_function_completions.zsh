# I have a bunch of functions in ~/dotfiles/zsh/functions/ and let's make using them much nicer with
# appropriate tab completion!

function _systemd_units()
{
    # Load the various helper functions that the _systemctl compdef defines,
    # if it hasn't been already
    # TODO: this is a hack, the first time it runs it will return the regular systemctl completion
    (( $+functions[_systemctl_all_units] )) || _systemctl
    _systemctl_all_units

    _wanted systemd-units expl unit \
        compadd "$@" -a - _sys_all_units
}

compdef _systemd_units svccurlog

if [ -n "${commands[fzf]}" ]; then
    compdef fancy-pid-complete pidenv
    compdef fancy-pid-complete pidstarted
    compdef fancy-pid-complete pidof
fi
