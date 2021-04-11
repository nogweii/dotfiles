function! health#editorconfig#check() abort
  call health#report_start('searching for external editor config core')

  let l:bin_found = executable("editorconfig")
  if l:bin_found
    call health#report_ok("found `editorconfig` in $PATH: " . exepath("editorconfig"))
  else
    call health#report_warn("Can not find `editorconfig` in $PATH, falling back to pure vimscript core", ['install the editorconfig-core-c package from extra'])
  endif
endfunction
