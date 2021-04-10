function! health#hexokinase#check() abort
  call health#report_start('searching for hexokinase binary')

  let l:bin_found = executable("hexokinase")
  if l:bin_found
    call health#report_ok("found `hexokinase` in $PATH: " . exepath("hexokinase"))
  else
    call health#report_error("Can not find `hexokinase` in $PATH", ['install the hexokinase-git package from AUR')
  endif
endfunction
