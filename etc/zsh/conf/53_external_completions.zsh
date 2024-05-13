# Additional shell completion functions from system packages

#########################
# fzf
#########################

# For Linux installs
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
# For MacOS homebrew installs
[ -f /opt/homebrew/opt/fzf/shell/completion.zsh ] && source /opt/homebrew/opt/fzf/shell/completion.zsh
[ -f /opt/homebrew/opt/fzf/shell/key-bindings.zsh ] && source /opt/homebrew/opt/fzf/shell/key-bindings.zsh

function docker_desktop_for_mac() {
  unfunction docker_desktop_for_mac
  #########################
  # Docker Desktop for Mac
  #########################

  if [ -d /Applications/Docker.app/Contents/Resources/etc ]; then
    source /Applications/Docker.app/Contents/Resources/etc/docker.zsh-completion
    source /Applications/Docker.app/Contents/Resources/etc/docker-compose.zsh-completion

    compdef _docker docker
    compdef _docker dockerd
    compdef _docker-compose docker-compose
  fi
}
delayed_init_functions+=(docker_desktop_for_mac)
