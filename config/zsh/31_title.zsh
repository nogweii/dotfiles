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

function preexec () {
  local CMD="${1[(wr)^(*=*|sudo|-*|s)]}"
  termtitle "%n@%M:%~ ($CMD)"
}

