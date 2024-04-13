case $TERM in
  termite|*xterm*|rxvt*|(dt|k|E)term|gnome*|konsole*|alacritty*)
    termtitle () {
      if [[ -n "${SSH_TTY}" ]]; then
        ssh_host="<%m> "
      fi
      print -Pn "\e]0;$ssh_host${@}\a"
    }
  ;;

  screen*|tmux*)
    termtitle () {
      if [[ -n "${SSH_TTY}" ]]; then
        ssh_host="<%m> "
      fi
      print -Pn "\e]83;title \"$ssh_host${@}\"\a"
      print -Pn "\e]0;$ssh_host${@}\a"
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
