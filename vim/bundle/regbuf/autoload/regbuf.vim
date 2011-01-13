" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Load Once {{{
if exists('s:loaded') && s:loaded
    finish
endif
let s:loaded = 1
" }}}
" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}



" Variables
let s:INVALID_REGISTER = -1
lockvar s:INVALID_REGISTER

let s:opened_bufnr = -1
let s:edit_bufnr = -1

function s:SID()
    return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun
let s:SID_PREFIX = s:SID()
delfunc s:SID



function! regbuf#open() "{{{
    augroup regbuf
        autocmd!
    augroup END

    let s:opened_bufnr = bufnr('%')
    autocmd regbuf BufWinLeave <buffer> let s:opened_bufnr = -1

    call s:create_buffer('regbuf:registers', g:regbuf_open_command, 'nofile')
    setlocal modifiable
    call s:write_registers()
    setlocal nomodifiable

    if g:regbuf_show_preview
        autocmd regbuf WinEnter,CursorMoved <buffer> call s:preview_register()
    endif
    autocmd regbuf BufWinLeave <buffer> call s:close_all_child_windows()
    autocmd regbuf WinLeave    <buffer> call s:close_preview_window()

    nnoremap <silent><buffer> <Plug>(regbuf-yank)  :<C-u>call <SID>do_operate('buf_yank')<CR>
    nnoremap <silent><buffer> <Plug>(regbuf-paste) :<C-u>call <SID>do_operate('buf_paste')<CR>
    nnoremap <silent><buffer> <Plug>(regbuf-swap)  :<C-u>call <SID>do_operate('buf_swap')<CR>
    nnoremap <silent><buffer> <Plug>(regbuf-paste-buffer)   :<C-u>call <SID>do_operate('buf_paste_buffer')<CR>
    nnoremap <silent><buffer> <Plug>(regbuf-paste-buffer-noclose)   :<C-u>call <SID>do_operate('buf_paste_buffer_noclose')<CR>
    nnoremap <silent><buffer> <Plug>(regbuf-paste-buffer-nodelete)   :<C-u>call <SID>do_operate('buf_paste_buffer_nodelete')<CR>
    nnoremap <silent><buffer> <Plug>(regbuf-edit)  :<C-u>call <SID>do_operate('buf_edit')<CR>
    nnoremap <silent><buffer> <Plug>(regbuf-close)  :<C-u>close<CR>
    if !g:regbuf_no_default_keymappings
        nmap <buffer> y <Plug>(regbuf-yank)
        nmap <buffer> p <Plug>(regbuf-paste)
        nmap <buffer> s <Plug>(regbuf-swap)
        nmap <buffer> bp <Plug>(regbuf-paste-buffer)
        nmap <buffer> e <Plug>(regbuf-edit)

        nmap <buffer> <CR>      <Plug>(regbuf-edit)
        nmap <buffer> q         <Plug>(regbuf-close)
        nmap <buffer> <Esc>     <Plug>(regbuf-close)
    endif

    setfiletype regbuf
endfunction "}}}
function! s:write_registers() "{{{
    let save_lang = v:lang
    lang messages C
    try
        redir => output
            silent registers
        redir END
    finally
        execute 'lang messages' save_lang
    endtry
    let lines = split(output, '\n')
    call remove(lines, 0)    " First line must be "--- Registers ---"
    call setline(1, lines)
    setlocal nomodified
endfunction "}}}
function! s:close_all_child_windows() "{{{
    let winnr = winnr()

    if bufwinnr(s:edit_bufnr) !=# -1
        execute bufwinnr(s:edit_bufnr) 'wincmd w'
        close
    endif

    call s:close_preview_window()

    execute winnr 'wincmd w'
endfunction "}}}

function! s:do_operate(opfunc) "{{{
    let s:op_register = v:register
    let &opfunc = "\<SNR>" . s:SID_PREFIX . '_' . a:opfunc
    normal! g@g@

    let winnr = bufwinnr(bufnr('regbuf:registers'))
    if winnr == -1    " regbuf:registers buffer is not displayed
        return
    endif
    let prevwinnr = -1
    if winnr != winnr()
        let prevwinnr = winnr()
        execute winnr 'wincmd w'
    endif

    setlocal modifiable
    call s:write_registers()
    setlocal nomodifiable

    if prevwinnr !=# -1
        execute prevwinnr 'wincmd w'
    endif
endfunction "}}}

function! s:buf_yank(...) "{{{
    let regname = s:get_regname_on_cursor()
    if regname ==# s:INVALID_REGISTER
        return
    endif
    let [value, type] = [getreg(regname, 1), getregtype(regname)]
    let given_regname = s:op_register != '' ? s:op_register : '"'
    call setreg(given_regname, value, type)

    redraw
    echo 'Yanked' regname 'register to' given_regname 'register.'
endfunction "}}}

function! s:buf_paste(...) "{{{
    let regname = s:get_regname_on_cursor()
    if regname ==# s:INVALID_REGISTER
        return
    endif
    let given_regname = s:op_register != '' ? s:op_register : '"'
    let [value, type] = [getreg(given_regname, 1), getregtype(given_regname)]
    call setreg(regname, value, type)

    redraw
    echo 'Pasted' given_regname 'register to' regname 'register.'
endfunction "}}}

function! s:buf_swap(...) "{{{
    let regname = s:get_regname_on_cursor()
    if regname ==# s:INVALID_REGISTER
        return
    endif
    let [value, type] = [getreg(regname, 1), getregtype(regname)]
    let given_regname = s:op_register != '' ? s:op_register : '"'
    let [given_value, given_type] = [getreg(given_regname, 1), getregtype(given_regname)]
    call setreg(regname, given_value, given_type)    " Yank to the register on cursor.
    call setreg(given_regname, value, type)    " Yank to given register by keymapping.

    redraw
    echo 'Swapped' regname 'and' given_regname 'register.'
endfunction "}}}

function! s:buf_paste_buffer(...) "{{{
    call s:paste_buffer()
endfunction "}}}
function! s:buf_paste_buffer_noclose(...) "{{{
    let save = g:regbuf_paste_buffer_noclose
    let g:regbuf_paste_buffer_noclose = 1
    try
        call s:paste_buffer()
    finally
        let g:regbuf_paste_buffer_noclose = save
    endtry
endfunction "}}}
function! s:buf_paste_buffer_nodelete(...) "{{{
    let save = g:regbuf_paste_buffer_nodelete
    let g:regbuf_paste_buffer_nodelete = 1
    try
        call s:paste_buffer()
    finally
        let g:regbuf_paste_buffer_nodelete = save
    endtry
endfunction "}}}
function! s:paste_buffer() "{{{
    let regname = s:get_regname_on_cursor()
    if regname ==# s:INVALID_REGISTER
        return
    endif

    let winnr = bufwinnr(s:opened_bufnr)
    if winnr ==# -1
        echohl WarningMsg
        echomsg 'original buffer is closed...'
        echohl None
        return
    endif

    let prevwinnr = winnr()
    execute winnr 'wincmd w'
    if g:regbuf_paste_buffer_nodelete
        let pastecmd = getregtype(regname) == 'v' ? 'P' : 'p'
        execute 'normal!' '"' . regname . pastecmd
    else
        %delete _
        call setline(1, split(getreg(regname, 1), '\n'))
    endif
    execute prevwinnr 'wincmd w'

    if !g:regbuf_paste_buffer_noclose
        quit    " will call s:close_all_child_windows().
    endif

    redraw
    echo 'Pasted' regname 'register to buffer(' . bufname(s:opened_bufnr) . ').'
endfunction "}}}

function! s:buf_edit(...) "{{{
    let regname = s:get_regname_on_cursor()
    if regname ==# s:INVALID_REGISTER
        return
    endif
    call s:open_register_buffer(regname)
endfunction "}}}
function! s:open_register_buffer(regname) "{{{
    call s:close_preview_window()

    let open_command =
    \   exists('g:regbuf_edit_open_command') ?
    \       g:regbuf_edit_open_command :
    \       g:regbuf_open_command
    let s:edit_bufnr = s:create_buffer('regbuf:edit:@' . a:regname, open_command, 'acwrite')
    setlocal modifiable
    call s:write_register_value(a:regname)

    command!
    \   -bar -buffer -nargs=*
    \   RegbufEditApply
    \   call s:buf_edit_apply(<f-args>)
    command!
    \   -bar -buffer
    \   RegbufEditCancel
    \   close

    if !g:regbuf_no_default_edit_autocmd
        augroup regbuf-edit
            autocmd!
            autocmd BufWritecmd <buffer> RegbufEditApply
            autocmd BufWinLeave <buffer> let s:edit_bufnr = -1
        augroup END
    endif

    nnoremap <buffer> <Plug>(regbuf-edit-cancel) :<C-u>RegbufEditCancel<CR>
    nnoremap <buffer> <Plug>(regbuf-edit-apply)  :<C-u>RegbufEditApply<CR>

    setfiletype regbuf-edit

    redraw
    echo 'Start editing' a:regname 'register.'
endfunction "}}}
function! s:write_register_value(regname) "{{{
    %delete _
    let [value, _] = [getreg(a:regname, 1), getregtype(a:regname)]
    let b:regbuf_edit_regname = a:regname
    call setline(1, split(value, '\n'))
    setlocal nomodified
endfunction "}}}

function! s:buf_edit_apply() "{{{
    if !exists('b:regbuf_edit_regname')
        echoerr "b:regbuf_edit_regname is deleted by someone. Can't continue applying..."
        return
    endif

    let lines = getline(1, '$')
    let [value, type] = [join(lines, "\n"), s:detect_regtype(lines)]
    call setreg(b:regbuf_edit_regname, value, type)

    setlocal nomodified
endfunction "}}}
function! s:detect_regtype(lines) "{{{
    " TODO
    if len(a:lines) > 1
        return 'V'
    else
        return 'v'
    endif
endfunction "}}}


function! s:create_buffer(name, open_command, buftype) "{{{
    let winnr = bufwinnr(bufnr(a:name))
    if winnr ==# -1    " not displayed
        execute a:open_command a:name
        setlocal bufhidden=unload noswapfile nobuflisted
        let &l:buftype = a:buftype
    else
        execute winnr 'wincmd w'
    endif
    return bufnr('%')
endfunction "}}}


function! s:get_regname_on_cursor() "{{{
    if expand('%') !=# 'regbuf:registers'
        return s:INVALID_REGISTER
    endif
    let line = getline('.')
    if line[0] !=# '"'
        return s:INVALID_REGISTER
    endif
    return line[1]
endfunction "}}}


function! s:preview_register() "{{{
    let regname = s:get_regname_on_cursor()
    if regname ==# s:INVALID_REGISTER
        return
    endif

    if bufwinnr(bufnr('regbuf:preview')) ==# -1
        call s:create_buffer('regbuf:preview', 'pedit', 'nofile')
    endif

    " :pedit does not change current window.
    " So we must jump into that window manually
    " and write register value.
    let prev_winnr = winnr()
    for winnr in range(1, winnr('$'))
        if getwinvar(winnr, '&previewwindow')
            execute winnr 'wincmd w'
            setlocal modifiable
            call s:write_register_value(regname)
            setlocal nomodifiable
            execute prev_winnr 'wincmd w'

            break
        endif
    endfor
endfunction "}}}


function! s:close_preview_window() "{{{
    if !g:regbuf_show_preview
        return
    endif
    pclose
endfunction "}}}


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
