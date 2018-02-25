" Append a vim modeline to the end of the file
function! s:AppendModeline()
  let save_cursor = getpos('.')
  " Build the modline out of syntax, tabstop, and shiftwidth settings
  let l:modeline =  ' vim: set filetype='.&syntax.' ts='.&tabstop.' sw='.&shiftwidth.':'
  " Use &commentstring so that the vim modeline isn't parsed in the file
  let l:modeline = substitute(&commentstring, '%s', l:modeline, '')
  " Reduce any multiple spaces into 1 (often when commentstring includes a
  " space as well)
  let l:modeline = substitute(l:modeline, '  ', ' ', 'g')

  " Append to the end of the file
  $put =l:modeline

  " Jump back to wherever the user's cursor was
  call setpos('.', save_cursor)

  silent! call repeat#set("\<Plug>(AppendModeline)", 0)
endfunction

nnoremap <silent> <Plug>(AppendModeline) :call <SID>AppendModeline()<CR>
nmap <silent> <Leader>ml <Plug>(AppendModeline)
