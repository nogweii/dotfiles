# Integrate using atuin (https://atuin.sh/) with zsh
#
# This file is a merge and customization of:
# - The built-in zsh configuration (`atuin init zsh`)
# - https://gist.github.com/tyalie/7e13cfe2ec62d99fa341a07ed12ef7c0
# - https://github.com/takac/atuin/blob/zsh-ctrl-r-widget/atuin.isearch.zsh
# - https://github.com/zsh-users/zsh-history-substring-search
#
# It ends up doing the same job as histdb and zsh-history-substring-search

# Early return to skip evaluating the rest of the file when atuin isn't installed
if [[ -z $commands[atuin] ]]; then
   return
fi

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

add-zsh-hook preexec _atuin_preexec
add-zsh-hook precmd _atuin_precmd

zle -N atuin-search _atuin_search
zle -N atuin-search-vicmd _atuin_search_vicmd
zle -N atuin-search-viins _atuin_search_viins

bindkey -M emacs '^r' atuin-search
bindkey -M viins '^r' atuin-search-viins
bindkey -M vicmd '/' atuin-search

typeset -g _atuin_highlight_current=''
typeset -g _atuin_highlight_found='bg=#e2c792,fg=#252623,underline'
typeset -g _atuin_highlight_not_found='bg=red,fg=white,bold'

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
  _atuin-history-down-search

  _atuin-history-search-end
}

zle -N atuin-history-up
zle -N atuin-history-down

bindkey '^[[A' atuin-history-up
bindkey '^[[B' atuin-history-down
bindkey -M vicmd k atuin-history-up
bindkey -M vicmd j atuin-history-down
bindkey -M viins '^P' atuin-history-up
bindkey -M viins '^N' atuin-history-down

#-----------END main---------------

#----------------------------------
# implementation details
#----------------------------------

_atuin-history-search-begin() {
  # assume we will not render anything
  _atuin_history_refresh_display=
  _atuin_highlight_current=''

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
  # this is some arbitrary text that zle will ignore, but allows us to find our highlight later on
  local highlight_memo='memo=atuin-history-search'

  # draw buffer if requested
  if [[ $_atuin_history_refresh_display -eq 1 ]]; then
    BUFFER="$_atuin_history_search_result"

    # remove the custom atuin highlight, if it exists
    region_highlight=( "${(@)region_highlight:#*${highlight_memo}*}" )

    # move the cursor to the end of the command
    CURSOR="${#BUFFER}"
  fi

  if [[ -z $_atuin_history_search_query ]]; then
	  return 0
  fi

  # highlight command line using zsh-syntax-highlighting,
  # before adding our own region
  _zsh_highlight

  # (i) get the index of a pattern, (#i) does it case insensitively
  local matching_index="${BUFFER[(i)(#i)${_atuin_history_search_query}]}"
  # if a string matches, it will be a number less than the length of the string.
  # values greater than the length means it didn't match
  if [[ $matching_index -le ${#BUFFER} ]]; then
    # append our region to zle's highlights (see region_highlight in zshzle(1) for details)
    region_highlight+=(
      # it was found! tell zle to color this region of the text (but it's 0 indexed)
      "$(($matching_index - 1)) $(($matching_index + ${#_atuin_history_search_query} - 1)) ${_atuin_highlight_current},${highlight_memo}"
    )
  fi

  # redraw the line
  zle -R

  # wait for timeout (1 sec) or user input before removing the search highlight
  read -k -t 1 && zle -U $REPLY
  region_highlight=( "${(@)region_highlight:#*${highlight_memo}*}" )

  return 0
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
    _atuin_highlight_current=$_atuin_highlight_not_found
    return 1
  fi

  _atuin_history_refresh_display=1
  _atuin_history_search_result="$search_result"
  _atuin_highlight_current=$_atuin_highlight_found
  return 0
}

_atuin-history-down-search() {
  # we can't go below 0
  if [[ $_atuin_history_match_index -le 0 ]]; then
    _atuin_highlight_current=$_atuin_highlight_not_found
    return 1
  fi

  _atuin_history_refresh_display=1
  _atuin_history_match_index+=-1

  offset=$((_atuin_history_match_index-1))
  _atuin_history_search_result=$(_atuin-history-do-search $offset "$_atuin_history_search_query")

  _atuin_highlight_current=$_atuin_highlight_found
  return 0
}

_atuin-history-do-search() {
  if [[ $1 -ge 0 ]]; then
    atuin search --filter-mode=global --search-mode full-text \
      --limit 1 --offset $1 --format "{command}" \
      "$2"
  fi
}

#------END implementation----------
