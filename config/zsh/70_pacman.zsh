########################
# Pacman

if [ -x /usr/bin/clyde ] ; then

    sup-clyde() {
        case $1 in
            (-Ss | -Si | -Q* | -T | -*h* | --help)
                /usr/bin/clyde "$@" ;;
            (-S* | -R* | -U | *)
                /usr/bin/sudo /usr/bin/clyde --needed "$@" || return $? ;;
        esac
    }
    alias clyde="sup-clyde"

    # Pacman completion for clyde
    compdef clyde=pacman
    compdef sup-clyde=pacman
fi
