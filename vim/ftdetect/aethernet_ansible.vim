function! s:isAetherAnsible()
  let filepath = expand("%:p")
  let filename = expand("%:t")
  if filepath =~ 'aethernet' && filename =~ '.ya\?ml$' | return 1 | en

  return 0
endfunction

au BufNewFile,BufRead * if s:isAetherAnsible() | set ft=ansible | en
