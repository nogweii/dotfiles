# A whole file filled with internal utility functions. Meant for zsh's use only,
# not manually invoked by the user.

# rake autocompletion from:
# http://weblog.rubyonrails.org/2006/3/9/fast-rake-task-completion-for-zsh
_rake_does_task_list_need_generating () {
  if [ ! -f .rake_tasks ]; then return 0;
  else
    accurate=$(stat -c%Y .rake_tasks)
    changed=$(stat -c%Y Rakefile)
    return $(expr $accurate '>=' $changed)
  fi
}

_rake () {
  if [ -f Rakefile ]; then
    if _rake_does_task_list_need_generating; then
      rake --silent --tasks | cut -d' ' -f2 2>/dev/null >.rake_tasks
    fi
    compadd `cat .rake_tasks`
  fi
}

_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1 # Because we didn't really complete anything
}

# Based off http://gist.github.com/172292. Idea by @defunkt
# gist it! http://gist.github.com/172323 (zsh fork)
function _ruby_or_irb() {
    if [[ "$1" == "" ]]; then
        command irb -f -I"${DOTSDIR}/config/irb" -r irb_conf
    else
        command ruby $@
    fi
}

# Uses xdotool & the environment variable $WINDOWID to check if the current
# window is focused in X11. Also requires $DISPLAY to be set (used as a signal
# that X11 is indeed running).
#
# Returns 1 or 0, so you can easily to __is-my-window-focused && notify-send or
# similar.
function __is-my-window-focused() {
  [[ -n "${WINDOWID}" ]] || return 1
  [[ -n "${DISPLAY}" ]] || return 1
  [[ ${$(xprop -root -notype -format _NET_ACTIVE_WINDOW 32i ' $0\n' _NET_ACTIVE_WINDOW):1} = $WINDOWID ]]
}

# Passing 1 or 2 args in, test if the first parameter is a subdirectory. If you
# don't pass in the second parameter, it defaults to testing if the path is a
# subdirectory of $PWD. Does not read links.
function is-path-subdirectory() {
  # Convert both arguments to absolute paths (the :A modifier)
  full_path_in_question="${1:A}"
  potential_parent_path="${2:-$PWD:A}"

  # Do a string prefix match of the full path and the potential parent path
  test_result=$(test "${full_path_in_question:0:${#potential_parent_path}}" = "${potential_parent_path}")

  return $test_result
}
