########################
# Pacman

if [[ ! -x "${commands[pacman]}" ]] ; then
    # We are running on a not-arch machine, skip setting up pacman.
    return
fi

_smart_sudo_pacman() {
    case $1 in
        (-Ss) # getting pretty colors regardless if clyde has been installed
            command pacman "$@"
        ;;
        (-Si | -Sg | -Sl | -Q* | -T | -*h* | --help)
            command pacman  "$@"
        ;;
        (-S* | -R* | -U* | *)
            if [ -x /usr/bin/pacmatic ] ; then
                # we're doing sudo, so this is when pacmatic behaves
                binary="pacmatic"
                /usr/bin/sudo pacman_program=$pacman_program \
                    pacman $pacman_opts $@ || return $?
            else
                /usr/bin/sudo pacman $pacman_opts $@ || return $?
            fi
        ;;
    esac
}

compdef _smart_sudo_pacman=pacman
alias pacman='_smart_sudo_pacman'
