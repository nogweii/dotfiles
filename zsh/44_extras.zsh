source ${DOTSDIR}/zsh/plugins/many-dots-magic.zsh

source ${DOTSDIR}/zsh/plugins/vi-more-increment/vi-increment.zsh
source ${DOTSDIR}/zsh/plugins/vi-more-quote/vi-quote.zsh

source ${DOTSDIR}/zsh/plugins/autopair/autopair.zsh
# Add an additional pair character:
AUTOPAIR_PAIRS+=("<" ">")
# To remove pairs, use:
#unset 'AUTOPAIR_PAIRS[<]'
# Then, do all of the key bindings:
autopair-init

source ${DOTSDIR}/zsh/plugins/system-clipboard/zsh-system-clipboard.zsh

# Bind Y to yank until end of line, not the whole line.
bindkey -M vicmd Y zsh-system-clipboard-vicmd-vi-yank-eol
