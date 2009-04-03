" CleverTabs v1.0.2: Martin Spevak 2008
" contact: martin.spevak [at] mobitola.sk
"
" Script solving indentation problem. Main idea is
" have tabs ("\t") at the beginning of line and
" after first another character like "\t" use spaces 
" (like expandtab). This is useful for code readability.
" I'm trying to join these two ways:
" * http://www.movement.uklinux.net/docs/whytabs/
" * http://www.derkarl.org/why_to_tabs.html
" with using the best ideas from both.
"
" main() {
"     printf("hello");    //my comment
"     printf("world!");   //my comment 2
" }
"
" Before printf are tabs (this is block) and before comments
" are tabs converted into spaces. After you change
" tabstop (for tabelator size), part with comments will
" have still same format, becuase tabs are only on line beginnig.
"
" instalation:
" paste content from CleverTabs.vim into .vimrc (or use vim/plugin
" directory).
"
" note:    parameter for CleverTabs is shiftwidth in spaces
" note2:   this is my first script in vim, please be patient

" clever tabs (tabs only on the line beginning)
function! CleverTabs(shiftwidth)
	let line = getline('.')[:col('.')-2]
	if col('.') == 1 || line =~ '^\t*$' || line =~ '^$'
		let z = "\t"
	else
		let space = ""
		let shiftwidth = a:shiftwidth
		let shiftwidth = shiftwidth - ((virtcol('.')-1) % shiftwidth)

		while shiftwidth > 0
			let shiftwidth = shiftwidth - 1
			let space = space . ' '
		endwhile

		let z = space
	endif

	return z
endfunction "CleverTabs

" map tab key to function
imap <silent> <Tab> <C-r>=CleverTabs(4)<cr>

" highlight tabs in code
highlight ExtraWhitespace ctermbg=07
match ExtraWhitespace /\t\+/
