# -*- mode: sh -*-
#
# manydots-magic - zle tweak for emulating "..."=="../.." etc.
#
# Copyright (c) 2011, 2012 Akinori MUSHA
# Licensed under the 2-clause BSD license.
#
# This tweak helps input ancestor directories beyond the parent (`..')
# in a handy way.  You can just type triple dots to input `../..',
# quadruple dots to `../../..', etc..
#
#     % .. [Hit <.>]
#     % ../.. [Hit <.>]
#     % ../../.. [Hit <^H>]
#     % ../.. [Hit <^H>]
#     % ..
#
# As you see above, each of the `/..' parts complemented by this tweak
# can be deleted by a single invocation of the backward-delete-char
# command, only if invoked right after the magic happens.
#
#     % .. [Hit </><.><.>]
#     % ../.. [Hit <^H>]
#     % ../.
#
# Usage:
#     autoload -Uz manydots-magic
#     manydots-magic
#

# Source:
#  - https://github.com/knu/zsh-manydots-magic
#  - https://github.com/twang817/zsh-manydots-magic

manydots-magic.self-insert() {
    emulate -L zsh
    setopt extended_glob warn_create_global typeset_silent \
        no_short_loops rc_quotes no_auto_pushd

    local si magic_count
    zstyle -s ':manydots-magic' self-insert-function si

    if [[ "$KEYS" == .* && "$LBUFFER" != *...* && "$LBUFFER" == *.. ]] && {
        local -a words
        words=("${(@Q)${(z)LBUFFER}}")
        # `...` is a wildcard operator in go
        [[ ${${(@)words[1,-2]}[(I)go]} = 0 ]] &&
        [[ $words[-1] == (|*[/=]|[\<\>=]\().. ]]
    }
    then
        [[ "$LASTWIDGET" == (self-insert|backward-delete-char|autosuggest-suggest) ]] &&
        zstyle -s ':manydots-magic' magic-count magic_count
        zstyle ':manydots-magic' magic-count $((magic_count+1))
        LBUFFER="$LBUFFER/."
        zle "$si"
        return
    fi

    # cancel expansion if it does not seem right
    if [[ "$KEYS" != [=/,:\;\|\&\<\>\(\)\[\]{}^~\'\"\`[:space:]]* &&
        "$LASTWIDGET" == (self-insert|backward-delete-char|autosuggest-suggest) && "$LBUFFER" == *../.. ]] && {
        zstyle -s ':manydots-magic' magic-count magic_count
        [[ "$magic_count" -gt 0 ]]
    }
    then
        repeat $magic_count LBUFFER="${LBUFFER%/..}"
        repeat $magic_count LBUFFER="$LBUFFER."
    fi

    zstyle ':manydots-magic' magic-count 0

    zle "$si"
}

manydots-magic.backward-delete-char() {
    emulate -L zsh
    setopt extended_glob warn_create_global typeset_silent \
        no_short_loops rc_quotes no_auto_pushd

    local bdc
    zstyle -s ':manydots-magic' backward-delete-char-function bdc

    if [[ "$LASTWIDGET" == (self-insert|backward-delete-char|autosuggest-suggest) && "$LBUFFER" == *../.. ]] && {
        local magic_count
        zstyle -s ':manydots-magic' magic-count magic_count
        [[ "$magic_count" -gt 0 ]]
    }
    then
        zstyle ':manydots-magic' magic-count $((magic_count-1))
        LBUFFER="${LBUFFER%..}"
    else
        zstyle ':manydots-magic' magic-count 0
    fi

    zle "$bdc"
}

manydots-magic.on() {
    emulate -L zsh
    setopt extended_glob warn_create_global typeset_silent \
        no_short_loops rc_quotes no_auto_pushd

    local si=$widgets[self-insert]
    [[ $si = "user:manydots-magic.self-insert" ]] &&
        return 0

    if [[ $si = "builtin" ]]; then
        si=.self-insert
    else
        si=${si#user:}
        zle -la "$si" || zle -N "$si"
    fi
    zstyle ':manydots-magic' self-insert-function "$si"
    zle -A manydots-magic.self-insert self-insert

    local bdc=$widgets[backward-delete-char]
    if [[ $bdc = "builtin" ]]; then
        bdc=.backward-delete-char
    else
        bdc=${bdc#user:}
        zle -la "$bdc" || zle -N "$bdc"
    fi
    zstyle ':manydots-magic' backward-delete-char-function "$bdc"
    zle -A manydots-magic.backward-delete-char backward-delete-char

    zstyle ':manydots-magic' magic-count 0

    return 0
}

manydots-magic.off() {
    emulate -L zsh
    setopt extended_glob warn_create_global typeset_silent \
        no_short_loops rc_quotes no_auto_pushd

    local si bdc

    zstyle -s ':manydots-magic' self-insert-function si
    [[ -n "$si" ]] && zle -A "$si" self-insert

    zstyle -s ':manydots-magic' backward-delete-char-function bdc
    [[ -n "$bdc" ]] && zle -A "$bdc" backward-delete-char

    zstyle ':manydots-magic' magic-count 0

    return 0
}

zle -N manydots-magic.self-insert
zle -N manydots-magic.backward-delete-char
zle -N manydots-magic.on
zle -N manydots-magic.off

manydots-magic() {
    manydots-magic.on
}

[[ -o kshautoload ]] || manydots-magic "$@"
