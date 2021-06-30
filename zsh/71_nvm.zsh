# Set up nvm if I have it installed
if [ -f /usr/share/nvm/init-nvm.sh ]; then
  export NVM_DIR="${HOME}/.local/nvm"
  source /usr/share/nvm/nvm.sh
  source /usr/share/nvm/install-nvm-exec
fi
