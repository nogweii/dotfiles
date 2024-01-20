# Zsh user configuration file

# Disable color and theme in dumb/basic terminals.
if [[ "$TERM" == 'dumb' || "$TERM" == "linux" ]]; then
  zstyle ':zsh' color 'no'
  zstyle ':zsh:prompt' theme 'off'
fi

# Hard coded path since we don't know what kind of environment we're in
local zshrc_snipplet
for zshrc_snipplet in $DOTSDIR/zsh/*.zsh ; do
        source $zshrc_snipplet
done

# And load local configuration overrides
[[ -r ~/.zshrc.local ]] && source ~/.zshrc.local

# Rebuild the named directories hash. Since I just created a bunch of variables,
# did a lot of scripting, and who knows what else, this hash table is populated
# with a lot of extra junk.
hash -dr

# Stop tracing in zsh
if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    zprof
    zmodload -u zsh/zprof
    exec 2>&3 3>&-
fi
