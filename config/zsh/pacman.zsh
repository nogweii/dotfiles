########################
# Pacman

PACMAN_OPTIONS=""
POWERPILL_OPTIONS=""

pacman_bin="$(whence pacman-color || whence pacman)"

if [[ -z "$(whence powerpill)" ]] ; then
    alias pacman="$pacman_bin $PACMAN_OPTIONS"
else
    alias pacman="$(whence powerpill) $POWERPILL_OPTIONS --pacman-bin $pacman_bin $PACMAN_OPTIONS"
fi

alias pacmanr="s $(whence pacman) -Rcnsu"
alias spacman="s $(whence pacman) -S"
