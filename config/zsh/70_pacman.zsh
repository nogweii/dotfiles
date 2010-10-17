########################
# Pacman

if [ -x /usr/bin/clyde ] ; then

    sup-clyde() {
        case $1 in
            (-Ss | -Si | -Sg | -Q* | -T | -*h* | --help)
                /usr/bin/clyde "$@" ;;
            (-S* | -R* | -U | *)
                /usr/bin/sudo /usr/bin/clyde --needed "$@" || return $? ;;
        esac
    }
    alias clyde="sup-clyde"

    # Pacman completion for clyde
    compdef clyde=pacman
    compdef sup-clyde=pacman

    # Short alias / function to search with clyde then pan through them with vim
    # TODO: Set up vim as a global pager, then pipe this output to $PAGER
    pss() {
        clyde -Ss $1 &> /tmp/pss.$1.pacmans && $EDITOR /tmp/pss.$1.pacmans
    }
fi
