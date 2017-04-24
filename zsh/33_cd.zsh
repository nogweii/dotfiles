# I have customized the normal process of changing into a directory a whole lot.
# The functions in this file do a lot of that heavy lifting.

# just type '...' to get '../..'
rationalise-dot() {
  local MATCH
  if [[ $LBUFFER =~ '(^|/| |	|'$'\n''|\||;|&)\.\.$' ]]; then
    LBUFFER+=/
    zle self-insert
    zle self-insert
  else
    zle self-insert
  fi
}
zle -N rationalise-dot

# When passed a file, cd into the directory in which it resides, instead of
# throwing an error.
function smart_cd () {
  if [[ -f $1 ]] ; then
    [[ ! -e ${1:h} ]] && return 1
    builtin cd ${1:h}
  else
    builtin cd ${1}
  fi
}

# Whenever I change directories, list the contents of them.
function _ls-on-cd() {
  emulate -L zsh
  if [ -z "$_quiet_cd" ]; then
    ls
  else
    unset _quiet_cd
  fi
}
add-zsh-hook chpwd _ls-on-cd

# when calling cd, either do one of a few things:
# - without any parameters, inside of a VCS repo, cd to the root directory
# - without any parameters, outside of a repo, cd to ~
# - with 1 parameter, allow for approximate spellings
# - with 2 parameters, replace all instances of $1 with $2 in the pwd
function cd () {
  local approx1 ; approx1=()
  local approx2 ; approx2=()
  # No parameters, or the first one begins with a '+' or '-'
  if (( ${#*} == 0 )) || [[ ${1} = [+-]* ]] ; then
    # Are we in a VCS dir? (git only, relies on vcs_info) + no parameters?
    if [[ -n $vcs_info_msg_0_ ]] && (( ${#*} == 0 )); then
      builtin cd $(git rev-parse --show-toplevel)
    else
      builtin cd "$@"
    fi
  elif (( ${#*} == 1 )) ; then
    approx1=( (#a1)${1}(N) )
    approx2=( (#a2)${1}(N) )
    if [[ -e ${1} ]] ; then
      smart_cd ${1}
    elif [[ ${#approx1} -eq 1 ]] ; then
      smart_cd ${approx1[1]}
    elif [[ ${#approx2} -eq 1 ]] ; then
      smart_cd ${approx2[1]}
    else
      print couldn\'t correct ${1}
    fi
  elif (( ${#*} == 2 )) ; then
    builtin cd $1 $2
  else
    print cd: too many arguments
  fi
}
