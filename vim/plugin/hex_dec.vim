" Thank you tip 27, "Convert between hex and decimal"
" http://vim.wikia.com/wiki/VimTip27

command! -nargs=? -range Dec2hex call s:Dec2hex(<line1>, <line2>, '<args>')
function! s:Dec2hex(line1, line2, arg) range
	if empty(a:arg)
		if histget(':', -1) =~# "^'<,'>" && visualmode() !=# 'V'
			let cmd = 's/\%V\<\d\+\>/\=printf("0x%x",submatch(0)+0)/g'
		else
			let cmd = 's/\<\d\+\>/\=printf("0x%x",submatch(0)+0)/g'
		endif
		try
			execute a:line1 . ',' . a:line2 . cmd
		catch
			echo 'Error: No decimal number found'
		endtry
	else
		echo printf('%x', a:arg + 0)
	endif
endfunction

command! -nargs=? -range Hex2dec call s:Hex2dec(<line1>, <line2>, '<args>')
function! s:Hex2dec(line1, line2, arg) range
	if empty(a:arg)
		if histget(':', -1) =~# "^'<,'>" && visualmode() !=# 'V'
			let cmd = 's/\%V0x\x\+/\=submatch(0)+0/g'
		else
			let cmd = 's/0x\x\+/\=submatch(0)+0/g'
		endif
		try
			execute a:line1 . ',' . a:line2 . cmd
		catch
			echo 'Error: No hex number starting "0x" found'
		endtry
	else
		echo (a:arg =~? '^0x') ? a:arg + 0 : ('0x'.a:arg) + 0
	endif
endfunction
