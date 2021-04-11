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
let fc['https?://[^/]+twitter\.com'] = { 'takeover': 'never', 'priority': 1 }
let fc['https?://app\.slack\.com'] = { 'takeover': 'never', 'priority': 1 }
let fc['https?://discord\.com'] = { 'takeover': 'never', 'priority': 1 }

if exists('g:started_by_firenvim')
  augroup firenvim_tweaks
    autocmd!
    autocmd UIEnter * ++once call s:FirenvimSettingTweaks(v:event)
    " Automatically save (and thus update the text area) when editing
    autocmd TextChanged * ++nested write
    autocmd TextChangedI * ++nested write

    autocmd BufEnter github.com_*.txt set filetype=markdown
    autocmd BufEnter stackoverflow.com_*.txt,stackexchange.com_*.txt,*.stackexchange.com_*.txt set filetype=markdown
    autocmd BufEnter www.reddit.com_*.txt set filetype=markdown

    autocmd BufEnter play.golang.org_*.txt set filetype=go
    autocmd BufEnter play.rust-lang.org_*.txt set filetype=rust
  augroup END
endif
