function! health#notify#check() abort
  call health#report_start('searching for notify-send to support integration with desktop notifications')

  if executable("notify-send")
    call health#report_ok("found `notify-send` in $PATH: " . exepath("notify-send"))
  else
    call health#report_warn("Can not find `notify-send` in $PATH, falling back to basic text messages in-editor", ['pacman -S libnotify'])
  endif
endfunction
