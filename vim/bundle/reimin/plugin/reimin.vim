"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" reimin - easily #include a header
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if exists("loaded_reimin")
  finish
endif
if v:version < 700
  echoerr "reimin: this plugin requires vim >= 7."
  finish
endif
let loaded_reimin = 1

if has("autocmd")
  augroup reimin
    autocmd!
    autocmd FileType c,cpp   call s:InitC()
    autocmd FileType ruby    call s:InitRuby()
    autocmd FileType python  call s:InitPython()
  augroup END
endif

function! s:InitC()
  let s:include_params = {
  \   'keyword': '#include',
  \   'delimiter': ' ',
  \   'substitute': [
  \     ['^<\(.*\)>$', '<\1>', ''],
  \     ['^<\(.*[^>]\)$', '<\1>', ''],
  \     ['^[<"][>"]\?$', '', ''],
  \     ['^\(".*"\)$', '\1', ''],
  \     ['^"\(.*[^"]\)$', '"\1"', ''],
  \     ['^\([^<"]\)\(.*\)', '"\1\2"', '']
  \   ],
  \   'prompt': 'Include: '
  \ }
endfunction

function! s:InitRuby()
  let s:include_params = {
  \   'keyword': 'require',
  \   'delimiter': ' ',
  \   'substitute': [
  \     ['^', "'", ''],
  \     ['$', "'", '']
  \   ],
  \   'prompt': 'Require: '
  \ }
endfunction

function! s:InitPython()
  let s:include_params = {
  \   'keyword': 'import',
  \   'delimiter': ' ',
  \   'substitute': [],
  \   'prompt': 'Import: '
  \ }
endfunction

function <SID>reiminMain(opts)
  let l:include = input(a:opts['prompt'])
  let l:include = substitute(l:include, "^\\s\\+\\|\\s\\+$", "", "g")
  let l:pos = search(a:opts['keyword'], "bnw") " FIXME: regex-escape l:prompt
  for pipe in a:opts['substitute']
    let l:include = substitute(l:include, pipe[0], pipe[1], pipe[2])
  endfor
  if( l:include != "" )
    let l:include = a:opts['keyword'] . a:opts['delimiter'] . l:include
    call append(l:pos, l:include)
  endif
endfunction

function <SID>print_deprecated()
  echohl ErrorMsg
  echo "This command is deprecated. Please use :Include instead."
  echohl None
endfunction

command IncludeSystem :call <SID>print_deprecated()
command IncludeLocal  :call <SID>print_deprecated()
command Include :call <SID>reiminMain(s:include_params)
