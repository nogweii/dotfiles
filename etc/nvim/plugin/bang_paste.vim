fu! <SID>SetRegisterBang(v) range
  if a:v == 1
    normal gv
  endif
  let l:t = &shellredir
  let &shellredir = ">%s\ 2>/dev/tty"
  let @" = join(systemlist(input("\"!")), " ")
  let &shellredir = l:t
endf

nnoremap "! :call <SID>SetRegisterBang(0)<cr>
xnoremap "! :call <SID>SetRegisterBang(1)<cr>
