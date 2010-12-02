" Only show tabs if we are using spaces for padding. (YAIFA determines this)
function YAIFA_showtab()
  if g:showtab
    execute 'set listchars-=tab:\ \ '
    execute 'set listchars+=tab:'.nr2char(9655).nr2char(160)
  else
    execute 'set listchars-=tab:'.nr2char(9655).nr2char(160)
    execute 'set listchars+=tab:\ \ '
  endif
endfunction
au BufRead * call YAIFA_showtab()
