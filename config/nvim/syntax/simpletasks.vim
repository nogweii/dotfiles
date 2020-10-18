if exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'simpletasks'
endif

silent! syntax include @markdown syntax/markdown.vim
unlet! b:current_syntax

syn case match
syn sync fromstart

let s:regesc = '[]()?.*@='

function! s:CreateMatch(name, regex)
  exec 'syn match ' . a:name . ' "' . a:regex . '" contained'
endfunc

call s:CreateMatch('simpleTasksPendingMarker', '^\s*' . escape(b:SimpleTasksMarkerPending, s:regesc))
call s:CreateMatch('simpleTasksCompletedMarker', '^\s*' . escape(b:SimpleTasksMarkerDone, s:regesc))

syn region simpleTasksTask start=/^\s*/ end=/$/ oneline keepend contains=simpleTasksPendingMarker,simpleTasksCompletedMarker,@markdown,@spell
syn match simpleTasksProject "^\s*.*:\s*$"

hi def link simpleTasksPendingMarker Comment
hi def link simpleTasksCompletedMarker String
hi def link simpleTasksProject htmlH1
