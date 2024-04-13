" A quick function to find all of the code tags in the project
" https://en.wikipedia.org/wiki/Comment_(computer_programming)#Tags
" SPDX-License-Identifier: WTFPL

if !exists('g:comment_tags')
  let g:comment_tags = [
  \  'BUG',
  \  'FIXME',
  \  'HACK',
  \  'NOTE',
  \  'OPTIMIZE',
  \  'TODO',
  \  'UNDONE',
  \  'XXX',
  \ ]
endif

function s:SearchTags()
  call setqflist([])

  let tag_regex = join(g:comment_tags, '\|')
  execute 'silent :grep! "\b(' . tag_regex . ')\b"'

  " show results
  cwindow
endfunction

command CodeTagSearch call s:SearchTags()
