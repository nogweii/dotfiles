if !exists(':Tabularize')
  finish
endif

" Left-align ruby symbol hashes (1.9+) on the colon
AddTabularPattern! rb_sym_hash /\w:\zs/l0l1
" Left-align generic ruby hashes on the hashrocket
AddTabularPattern! rb_hash /^[^=]*\zs=>

" Mappings for ruby hash rocket and symbol hashes
"nnoremap <silent> <Leader>ahr :Tabularize rbhash<CR>
"vnoremap <silent> <Leader>ahr :Tabularize rbhash<CR>
"nnoremap <silent> <Leader>ahs  :Tabularize rbshash<CR>
"vnoremap <silent> <Leader>ahs  :Tabularize rbshash<CR>

" Nice alignment for | based tables
"inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a
 
function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction
