# Integrate using atuin (https://atuin.sh/) with zsh
#
# This file is a merge and customization of:
# - The built-in zsh configuration (`atuin init zsh`)
# - https://gist.github.com/tyalie/7e13cfe2ec62d99fa341a07ed12ef7c0
#
# It ends up doing the same job as histdb and zsh-history-substring-search

zmodload zsh/datetime 2>/dev/null

export ATUIN_SESSION=$(atuin uuid)
ATUIN_HISTORY_ID=""

_atuin_preexec() {
    local id
    id=$(atuin history start -- "$1")
    export ATUIN_HISTORY_ID="$id"
    __atuin_preexec_time=${EPOCHREALTIME-}
}

_atuin_precmd() {
    local EXIT="$?" __atuin_precmd_time=${EPOCHREALTIME-}

    [[ -z "${ATUIN_HISTORY_ID:-}" ]] && return

    local duration=""
    if [[ -n $__atuin_preexec_time && -n $__atuin_precmd_time ]]; then
        printf -v duration %.0f $(((__atuin_precmd_time - __atuin_preexec_time) * 1000000000))
    fi

    (ATUIN_LOG=error atuin history end --exit $EXIT ${duration:+--duration=$duration} -- $ATUIN_HISTORY_ID &) >/dev/null 2>&1
    export ATUIN_HISTORY_ID=""
}

_atuin_search() {
    emulate -L zsh
    zle -I

    # swap stderr and stdout, so that the tui stuff works
    # TODO: not this
    local output
    # shellcheck disable=SC2048
    output=$(ATUIN_SHELL_ZSH=t ATUIN_LOG=error atuin search $* -i -- $BUFFER 3>&1 1>&2 2>&3)

    zle reset-prompt

    if [[ -n $output ]]; then
        RBUFFER=""
        LBUFFER=$output

        if [[ $LBUFFER == __atuin_accept__:* ]]
        then
            LBUFFER=${LBUFFER#__atuin_accept__:}
            zle accept-line
        fi
    fi
}
_atuin_search_vicmd() {
    _atuin_search --keymap-mode=vim-normal
}
_atuin_search_viins() {
    _atuin_search --keymap-mode=vim-insert
}

_atuin_up_search() {
    # Only trigger if the buffer is a single line
    if [[ ! $BUFFER == *$'\n'* ]]; then
        _atuin_search --shell-up-key-binding "$@"
    else
        zle up-line
    fi
}
_atuin_up_search_vicmd() {
    _atuin_up_search --keymap-mode=vim-normal
}
_atuin_up_search_viins() {
    _atuin_up_search --keymap-mode=vim-insert
}

add-zsh-hook preexec _atuin_preexec
add-zsh-hook precmd _atuin_precmd

zle -N atuin-search _atuin_search
zle -N atuin-search-vicmd _atuin_search_vicmd
zle -N atuin-search-viins _atuin_search_viins
zle -N atuin-up-search _atuin_up_search
zle -N atuin-up-search-vicmd _atuin_up_search_vicmd
zle -N atuin-up-search-viins _atuin_up_search_viins

# These are compatibility widget names for "atuin <= 17.2.1" users.
zle -N _atuin_search_widget _atuin_search
zle -N _atuin_up_search_widget _atuin_up_search

bindkey -M emacs '^r' atuin-search
bindkey -M viins '^r' atuin-search-viins
bindkey -M vicmd '/' atuin-search
bindkey -M emacs '^[[A' atuin-up-search
bindkey -M vicmd '^[[A' atuin-up-search-vicmd
bindkey -M viins '^[[A' atuin-up-search-viins
bindkey -M emacs '^[OA' atuin-up-search
bindkey -M vicmd '^[OA' atuin-up-search-vicmd
bindkey -M viins '^[OA' atuin-up-search-viins
bindkey -M vicmd 'k' atuin-up-search-vicmd

#!/usr/bin/env zsh
##############################################################################
#
# Copyright (c) 2023 Sophie Tyalie
# Copyright (c) 2023 @Nezteb
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
#  * Redistributions in binary form must reproduce the above
#    copyright notice, this list of conditions and the following
#    disclaimer in the documentation and/or other materials provided
#    with the distribution.
#
#  * Neither the name of the FIZSH nor the names of its contributors
#    may be used to endorse or promote products derived from this
#    software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
##############################################################################

#----------------------------------
# main
#----------------------------------

# global configuration
: ${ATUIN_HISTORY_SEARCH_FILTER_MODE='session'}

# internal variables
typeset -g -i _atuin_history_match_index
typeset -g _atuin_history_search_result
typeset -g _atuin_history_search_query
typeset -g _atuin_history_refresh_display

atuin-history-up() {
  _atuin-history-search-begin

  # iteratively use the next mechanism to process up if the previous didn't succeed
  _atuin-history-up-buffer ||
  _atuin-history-up-search

  _atuin-history-search-end
}

atuin-history-down() {
  _atuin-history-search-begin

  # iteratively use the next mechanism to process down if the previous didn't succeed
  _atuin-history-down-buffer ||
  _atuin-history-down-search ||
  zle _atuin_search_widget

  _atuin-history-search-end
}

zle -N atuin-history-up
zle -N atuin-history-down

bindkey '\eOA' atuin-history-up
bindkey '\eOB' atuin-history-down
bindkey '^[[A' atuin-history-up
bindkey '^[[B' atuin-history-down

#-----------END main---------------

#----------------------------------
# implementation details
#----------------------------------

_atuin-history-search-begin() {
  # assume we will not render anything
  _atuin_history_refresh_display=

  # If the buffer is the same as the previously displayed history substring
  # search result, then just keep stepping through the match list. Otherwise
  # start a new search.
  if [[ -n $BUFFER && $BUFFER == ${_atuin_history_search_result:-} ]]; then
    return;
  fi

  # Clear the previous result.
  _atuin_history_search_result=''

  # setup our search query
  if [[ -z $BUFFER ]]; then
    _atuin_history_search_query=
  else
    _atuin_history_search_query="$BUFFER"
  fi

  # reset search index
  _atuin_history_match_index=0
}

_atuin-history-search-end() {
  # if our index is <= 0 just print the query we started with
  if [[ $_atuin_history_match_index -le 0 ]]; then
    _atuin_history_search_result="$_atuin_history_search_query"
  fi

  # draw buffer if requested
  if [[ $_atuin_history_refresh_display -eq 1 ]]; then
    BUFFER="$_atuin_history_search_result"
    CURSOR="${#BUFFER}"
  fi

  # for debug purposes only
  #zle -R "mn: "$_atuin_history_match_index" / qr: $_atuin_history_search_result"
  #read -k -t 1 && zle -U $REPLY

}

_atuin-history-up-buffer() {
  # attribution to zsh-history-substring-search
  #
  # Check if the UP arrow was pressed to move the cursor within a multi-line
  # buffer. This amounts to three tests:
  #
  # 1. $#buflines -gt 1.
  #
  # 2. $CURSOR -ne $#BUFFER.
  #
  # 3. Check if we are on the first line of the current multi-line buffer.
  #    If so, pressing UP would amount to leaving the multi-line buffer.
  #
  #    We check this by adding an extra "x" to $LBUFFER, which makes
  #    sure that xlbuflines is always equal to the number of lines
  #    until $CURSOR (including the line with the cursor on it).
  #
  local buflines XLBUFFER xlbuflines
  buflines=(${(f)BUFFER})
  XLBUFFER=$LBUFFER"x"
  xlbuflines=(${(f)XLBUFFER})

  if [[ $#buflines -gt 1 && $CURSOR -ne $#BUFFER && $#xlbuflines -ne 1 ]]; then
    zle up-line-or-history
    return 0
  fi

  return 1
}

_atuin-history-down-buffer() {
  # attribution to zsh-history-substring-search
  #
  # Check if the DOWN arrow was pressed to move the cursor within a multi-line
  # buffer. This amounts to three tests:
  #
  # 1. $#buflines -gt 1.
  #
  # 2. $CURSOR -ne $#BUFFER.
  #
  # 3. Check if we are on the last line of the current multi-line buffer.
  #    If so, pressing DOWN would amount to leaving the multi-line buffer.
  #
  #    We check this by adding an extra "x" to $RBUFFER, which makes
  #    sure that xrbuflines is always equal to the number of lines
  #    from $CURSOR (including the line with the cursor on it).
  #
  local buflines XRBUFFER xrbuflines
  buflines=(${(f)BUFFER})
  XRBUFFER="x"$RBUFFER
  xrbuflines=(${(f)XRBUFFER})

  if [[ $#buflines -gt 1 && $CURSOR -ne $#BUFFER && $#xrbuflines -ne 1 ]]; then
    zle down-line-or-history
    return 0
  fi

  return 1
}

_atuin-history-up-search() {
  _atuin_history_match_index+=1

  offset=$((_atuin_history_match_index-1))
  search_result=$(_atuin-history-do-search $offset "$_atuin_history_search_query")

  if [[ -z $search_result ]]; then
    # if search result is empty, there's no more history
    # so just show the previous result
    _atuin_history_match_index+=-1
    return 1
  fi

  _atuin_history_refresh_display=1
  _atuin_history_search_result="$search_result"
  return 0
}

_atuin-history-down-search() {
  # we can't go below 0
  if [[ $_atuin_history_match_index -le 0 ]]; then
    return 1
  fi

  _atuin_history_refresh_display=1
  _atuin_history_match_index+=-1

  offset=$((_atuin_history_match_index-1))
  _atuin_history_search_result=$(_atuin-history-do-search $offset "$_atuin_history_search_query")

  return 0
}

_atuin-history-do-search() {
  if [[ $1 -ge 0 ]]; then
    atuin search --filter-mode "$ATUIN_HISTORY_SEARCH_FILTER_MODE" --search-mode prefix \
      --limit 1 --offset $1 --format "{command}" \
      "$2"
  fi
}

#------END implementation----------
