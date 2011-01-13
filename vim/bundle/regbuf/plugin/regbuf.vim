" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Load Once {{{
if exists('g:loaded_regbuf') && g:loaded_regbuf
    finish
endif
let g:loaded_regbuf = 1
" }}}
" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}


" NOTE: Remember g:regbuf_edit_open_command variable.
" (:help g:regbuf_edit_open_command)

if !exists('g:regbuf_open_command')
    let g:regbuf_open_command = 'new'
endif
if !exists('g:regbuf_no_default_keymappings')
    let g:regbuf_no_default_keymappings = 0
endif
if !exists('g:regbuf_no_default_edit_autocmd')
    let g:regbuf_no_default_edit_autocmd = 0
endif
if !exists('g:regbuf_show_preview')
    let g:regbuf_show_preview = 1
endif
if !exists('g:regbuf_paste_buffer_noclose')
    let g:regbuf_paste_buffer_noclose = 0
endif
if !exists('g:regbuf_paste_buffer_nodelete')
    let g:regbuf_paste_buffer_nodelete = 0
endif


command!
\   -bar
\   RegbufOpen
\   call regbuf#open()


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
