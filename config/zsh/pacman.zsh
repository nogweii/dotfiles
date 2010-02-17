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
