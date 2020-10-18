if exists("b:loaded_simple_tasks")
  finish
endif
let b:loaded_simple_tasks = 1

let b:SimpleTasksMarkerPending = '☐'
let b:SimpleTasksMarkerPending = '□'
let b:SimpleTasksMarkerDone = '✔'
let b:SimpleTasksMarkerDone = '✗'

setlocal nospell

nnoremap <buffer> <silent> O :call <SID>NewTask(-1)<CR>
nnoremap <buffer> <silent> o :call <SID>NewTask(1)<CR>
nnoremap <buffer> <silent> <CR> :call <SID>TaskToggle()<CR>
nnoremap <buffer> <silent> dm :call <SID>DeleteLineMarker()<CR>
inoremap <buffer> <silent> <Tab> <esc>>>la
inoremap <buffer> <silent> <S-Tab> <esc><<la

let s:regesc = '[]()?.*@='

let s:regProject = '^\s*.*:\s\?$'
let s:regMarker = join([escape(b:SimpleTasksMarkerPending, s:regesc), escape(b:SimpleTasksMarkerDone, s:regesc)], '\|')
let s:regCompleted = escape(b:SimpleTasksMarkerDone, s:regesc)

function! s:NewTask(direction)
  let l:line = getline('.')
  let l:isMatch = match(l:line, s:regProject)
  let l:text = b:SimpleTasksMarkerPending . ' '

  if a:direction == -1
    exec 'normal! O' . l:text
  else
    exec 'normal! o' . l:text
  endif

  if l:isMatch > -1
    normal >>
  endif

  startinsert!
endfunc

function! s:SetLineMarker(marker)
  let l:line = getline('.')
  let l:markerMatch = match(l:line, s:regMarker)
  if l:markerMatch > -1
    " there is a marker, replace it with the argument
    call cursor(line('.'), l:markerMatch + 1)
    exec 'normal R' . a:marker
  else
    " there is no marker, add it at the beginning of the line
    exec 'normal ^'
  endif
endfunc

function! s:DeleteLineMarker()
  " if there is a marker, swap it out.
  " If there is no marker, add it in at first non-whitespace
  let l:line = getline('.')
  let l:markerMatch = match(l:line, s:regMarker)
  if l:markerMatch > -1
    call cursor(line('.'), l:markerMatch + 1)
    normal 2x
  endif
endfunc

function! s:TaskToggle()
  let l:line = getline('.')
  let l:isMatch = match(l:line, s:regMarker)
  let l:doneMatch = match(l:line, s:regCompleted)

  if l:isMatch > -1
    if l:doneMatch > -1
      " this task is done, mark it as pending
      call s:SetLineMarker(b:SimpleTasksMarkerPending)
    else
      " this task is now finished, mark as such
      call s:SetLineMarker(b:SimpleTasksMarkerDone)
    endif
  endif
endfunc
