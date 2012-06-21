# Zsh user configuration file
#
# From the many uploaded zshrc's on the 'net.

for zshrc_snipplet in ~/.local/config/zsh/*.zsh ; do
    source $zshrc_snipplet
done

if [ "$COLORTERM" = "gnome-terminal" ]; then
    export TERM="xterm-256color"
fi
