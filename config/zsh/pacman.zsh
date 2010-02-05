########################
# Pacman

PACMAN_OPTIONS=""
POWERPILL_OPTIONS=""

if [[ "$(echo =pacman-color)" != "=pacman-color" ]] ; then
    pacman_bin==pacman-color
else
    pacman_bin==pacman
fi

if [[ "$(echo =powerpill)" != "=powerpill" ]] ; then
    alias pacman="$(echo =powerpill) $POWERPILL_OPTIONS --pacman-bin $pacman_bin $PACMAN_OPTIONS"
else
    alias pacman="$pacman_bin $PACMAN_OPTIONS"
fi

alias    pacmanr="s $(whence pacman) -Rcnsu"
alias    spacman="s $(whence pacman) -S"
