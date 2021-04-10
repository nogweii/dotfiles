if executable("rg")
  set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case\ --follow
  set grepformat=%f:%l:%c:%m,%f:%l:%m
endif
