########################
# Pacman

# default, fall-back.
pacman_program=/usr/bin/pacman

if [ -x /usr/bin/clyde ] ; then
    pacman_program=clyde
fi

export pacman_program

# Short alias / function to search with clyde then pan through them with vim
# TODO: Set up vim as a global pager, then pipe this output to $PAGER
pss() {
    $pacman_program -Ss $1 &> /tmp/pss.$1.pacmans && $EDITOR /tmp/pss.$1.pacmans
}

pacman-sudo() {
    binary=$pacman_program
    case $1 in
        (-Ss) # getting pretty colors regardless if clyde has been installed
            if [ "$binary" != "clyde" -a -x /usr/bin/pacsearch ] ; then
                shift ; # pop the `-Ss`
                binary="pacsearch"
            fi
            $binary "$@"
        ;;
        (-Si | -Sg | -Q* | -T | -*h* | --help)
            $binary "$@"
        ;;
        (-S* | -R* | -U | *)
            # we're doing sudo, so this is when pacmatic behaves
            binary="pacmatic"
            /usr/bin/sudo pacman_program=$pacman_program \
                $binary --needed "$@" || return $?
        ;;
    esac
}

compdef pacman-sudo=$pacman_program
compdef pacman=$pacman_program
alias pacman="pacman-sudo"
alias clyde="pacman-sudo"
