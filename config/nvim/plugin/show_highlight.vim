" Shows the current highlight name for the syntax region below the cursor.
" Useful to find out why this word is colored so weirdly!
function! s:ShowHighlight()
  let position = getpos(".")
  let syntax_id = synID(position[1], position[2], 1)
  let highlight_name = synIDattr(syntax_id, "name")
  let highlight_translated = synIDattr(synIDtrans(syntax_id), "name")
  if highlight_translated == ""
    echo "No highlight region found"
  else
    execute "echohl " . highlight_name | echon highlight_name | echohl None | echon " links to " . highlight_translated
    execute "highlight " . highlight_translated
  endif
endfunction

" And map the function
noremap <script> <silent> <Leader>hi :call <SID>ShowHighlight()<CR>

" Original binding from the internet
"nmap <silent> <leader>hi
" \ :echo "hi<".synIDattr(synID(line("."),col("."),1),"name").'>'
" \ . ' trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
" \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"
" \ <CR>:execute "highlight " .
" \ synIDattr(synIDtrans(synID(line("."),col("."),1)),"name")<CR>
