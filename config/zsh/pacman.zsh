########################
# Pacman

pacman_bin="$(whence pacman-color || whence pacman)"

if [[ -z "$(whence powerpill)" ]] ; then
    alias pacman="$pacman_bin"
else
    alias pacman="$(whence powerpill) --pacman-bin $pacman_bin"
fi

alias pacmanr="s $(whence pacman) -Rcnsu"
alias spacman="s $(whence pacman) -S"

# Pacman completion for clyde
compdef _pacman clyde=pacman

sup-clyde() {
    case $1 in
        (-Ss | -Si | -Q* | -T | -*h*)
            /usr/bin/clyde "$@" ;;
        (-S* | -R* | -U | *)
            /usr/bin/sudo /usr/bin/clyde "$@" || /bin/su -c /usr/bin/clyde "$@" || return $? ;;
    esac
}
alias clyde="sup-clyde"
