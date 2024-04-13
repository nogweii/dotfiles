source ${ZDOTDIR}/plugins/many-dots-magic.zsh

source ${ZDOTDIR}/plugins/vi-more-increment/vi-increment.zsh
source ${ZDOTDIR}/plugins/vi-more-quote/vi-quote.zsh

source ${ZDOTDIR}/plugins/autopair/autopair.zsh
# Add an additional pair character:
AUTOPAIR_PAIRS+=("<" ">")
# To remove pairs, use:
#unset 'AUTOPAIR_PAIRS[<]'
# Then, do all of the key bindings:
autopair-init
