" inoremap <expr> <Esc>      pumvisible() ? "\\<C-e>" : "\\<C-R>=delimitMate#Finish()\\<CR>\\<C-o>:echo 'esc'\\<Esc>"
imap     <expr> <CR>       pumvisible() ? "\<C-y>" : "<C-\><C-n>o\<Plug>DiscretionaryEnd"
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<PageDown>"
inoremap <expr> <PageUp>   pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<PageUp>"
"inoremap <Esc> <C-o>o:echo 'hi'
