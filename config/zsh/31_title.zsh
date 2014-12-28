case $TERM in
  termite|*xterm*|rxvt*|(dt|k|E)term|gnome*|konsole*)
    termtitle () { print -Pn "\e]0;${@}\a" }
  ;;

  screen*|tmux*)
    termtitle () {
      print -Pn "\e]83;title \"${@}\"\a"
      print -Pn "\e]0;${@}\a"
    }
  ;;

  *)
    # No-op default, to avoid errors when running within an unknown shell
    termtitle() { }
  ;;
esac

function _update_termtitle_preexec () {
  local CMD="${1[(wr)^(*=*|sudo|-*|s)]}"
  termtitle "%~ ($CMD)"
}

function _update_termtitle_precmd () {
  termtitle "%~"
}

add-zsh-hook preexec _update_termtitle_preexec
add-zsh-hook precmd _update_termtitle_precmd
