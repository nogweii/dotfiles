# Zsh user configuration file
#
# From the many uploaded zshrc's on the 'net.

local dot_path="~/.local"
local host_config=~/.zshrc-$(hostname)
if [[ -r $host_config ]]; then
        source $host_config
fi

for zshrc_snipplet in $dot_path/config/zsh/*.zsh ; do
        source $zshrc_snipplet
done

if [ "$COLORTERM" = "gnome-terminal" ]; then
    export TERM="xterm-256color"
fi
