########################
# Pacman

if [[ ! -x "${commands[pacman]}" ]] ; then
    # We are running on a not-arch machine, skip setting up pacman.
    return
fi

_smart_sudo_pacman() {
    case $1 in
        (-Fy)
            /usr/bin/sudo pacman $pacman_opts $@ || return $?
        ;;
        (-Si | -Ss | -Sg | -Sl | -Q* | -T | -F* | -*h* | --help)
            command pacman "$@"
        ;;
        (-S* | -R* | -U* | *)
            /usr/bin/sudo pacman $pacman_opts $@ || return $?
        ;;
    esac
}

compdef _smart_sudo_pacman=pacman
alias pacman='_smart_sudo_pacman'
