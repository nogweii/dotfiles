function! health#notify#check() abort
  call health#report_start('searching for notify-send to support integration with desktop notifications')

  let l:bin_found = executable("notify-send")
  if l:bin_found
    call health#report_ok("found `notify-send` in $PATH: " . exepath("notify-send"))
  else
    call health#report_info("Can not find `notify-send` in $PATH, falling back to basic text messages in-editor", ['pacman -S libnotify'])
  endif
endfunction
