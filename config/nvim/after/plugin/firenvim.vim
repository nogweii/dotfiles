function! s:FirenvimSettingTweaks(event) abort
  " No need for a tab bar when the window is probably only ~3 lines tall
  set showtabline=0
  exec ':BarbarDisable'
endfunction

" Global defaults for firenvim
let g:firenvim_config = { 
    \ 'globalSettings': {
        \ 'alt': 'all',
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'cmdline': 'neovim',
            \ 'content': 'text',
            \ 'priority': 0,
            \ 'selector': 'textarea:not([readonly])',
            \ 'takeover': 'always',
        \ },
    \ }
\ }

let fc = g:firenvim_config['localSettings']
let fc['https?://[^/]+twitter.com'] = { 'takeover': 'never', 'priority': 1 }

" Automatically save after a few seconds
let g:firenvim_auto_write = v:false
function! Firenvim_Write(timer) abort
  let g:firenvim_auto_write = v:false
  write
endfunction

function! Debounced_Firenvim_Write() abort
  if g:firenvim_auto_write
    return
  end
  let g:firenvim_auto_write = v:true
  call timer_start(10000, 'Firenvim_Write')
endfunction


if exists('g:started_by_firenvim')
  augroup firenvim_tweaks
    autocmd!
    autocmd UIEnter * ++once call s:FirenvimSettingTweaks(v:event)
    autocmd TextChanged * ++nested call Debounced_Firenvim_Write()
    autocmd TextChangedI * ++nested call Debounced_Firenvim_Write()

    autocmd BufEnter github.com_*.txt set filetype=markdown
    autocmd BufEnter stackoverflow.com_*.txt,stackexchange.com_*.txt,*.stackexchange.com_*.txt set filetype=markdown
    autocmd BufEnter www.reddit.com_*.txt set filetype=markdown

    autocmd BufEnter play.golang.org_*.txt set filetype=go
    autocmd BufEnter play.rust-lang.org_*.txt set filetype=rust
  augroup END
endif
