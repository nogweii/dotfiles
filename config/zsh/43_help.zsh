# A smarter help widget, this will look for a file before resorting to using
# man.
(( ${+aliases[run-help]} )) && unalias run-help ||:
autoload run-help

# Files in this directory were generated from the `Util/helpfiles` command,
# included in the source distribution of Zsh.
HELPDIR=${XDG_DATA_HOME}/zsh/helpfiles

# Hit 'K' in command mode to launch online help, like in regular vim! :)
bindkey -M vicmd K run-help
