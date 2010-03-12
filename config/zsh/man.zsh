DYNAMIC_MANPATH="$(
echo -n $(manpath)
    for MAN in /usr/lib/gems/*/gems/*/man; do
        echo -n :$MAN
    done
)"
alias mandb="mandb $DYNAMIC_MANPATH"
alias man="man -M$DYNAMIC_MANPATH"
alias whatis="whatis -M$DYNAMIC_MANPATH"
alias apropos="apropos -M$DYNAMIC_MANPATH"
