# Zsh user configuration file

# Check for the minimum supported version. Borrowed from prezto. Considering I
# use new enough features and expect as such, this seems like a safe bet.
min_zsh_version='4.3.10'
if ! autoload -Uz is-at-least || ! is-at-least "$min_zsh_version"; then
  print "zsh: old shell detected, minimum required: $min_zsh_version" >&2
  return 1
fi
unset min_zsh_version

# Disable color and theme in dumb/basic terminals.
if [[ "$TERM" == 'dumb' || "$TERM" == "linux" ]]; then
  zstyle ':zsh' color 'no'
  zstyle ':zsh:prompt' theme 'off'
fi

# Hard coded path since we don't know what kind of environment we're in
local dot_path="$HOME/.local"
for zshrc_snipplet in $dot_path/config/zsh/*.zsh ; do
        source $zshrc_snipplet
done

# And load local configuration overrides
[[ -r ~/.zshrc.local ]] && source $host_config ||:
