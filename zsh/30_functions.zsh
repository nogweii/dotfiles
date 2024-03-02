#!/bin/zsh

autoload -Uz add-zsh-hook
autoload colors zsh/terminfo

local function_file
# Prep every one of my custom functions to be autoloaded
for function_file in ${DOTSDIR}/zsh/functions/* ; do
  emulate zsh -c "autoload -RUz ${function_file}"
done

# Same as above, autoload all of these files.
# But then also load them into zle to be used as widgets
for function_file in ${DOTSDIR}/zsh/zle-widgets/* ; do
  emulate zsh -c "autoload -RUz ${function_file}"
  zle -N ${function_file:t}
done

unset function_file
