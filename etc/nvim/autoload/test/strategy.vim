" A custom vim-test strategy that is mostly a copy & paste of the neovim
" strategy, but much simpler and tweaked to how I want it to behave
function! test#strategy#mine(cmd) abort
  botright 15 new
  call termopen(split(a:cmd))
  startinsert
endfunction
