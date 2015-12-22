" A custom status line, handling all aspects of its display. Heavily based off
" of @blaenk's own status implementation, described on his blog
" http://www.blaenkdenum.com/posts/a-simpler-vim-statusline/

" Includes integration with a slightly modified version of buftabs.
" Uses custom highlights, all beginning with 'status'. See my personal color
" scheme, devolved.

function! Status(winnum, buftabs)
  let active = a:winnum == winnr()
  let bufnum = winbufnr(a:winnum)

  let stat = ''

  " this function just outputs the content colored by the
  " supplied colorgroup number, e.g. num = 2 -> User2
  " it only colors the input if the window is the currently
  " focused one

  function! Color(active, group, content)
    if a:active
      return '%#' . a:group . '#' . a:content . '%*'
    else
      return a:content
    endif
  endfunction

  " this handles alternative statuslines
  let usealt = 0

  let type = getbufvar(bufnum, '&buftype')
  let name = bufname(bufnum)

  let altstat = ''

  if type ==# 'help'
    let altstat .= '%#statusFlag# HELP %* ' . fnamemodify(name, ':t:r')
    let usealt = 1
  elseif name ==# '__Gundo__'
    let altstat .= ' Gundo'
    let usealt = 1
  elseif name ==# '__Gundo_Preview__'
    let altstat .= ' Gundo Preview'
    let usealt = 1
  elseif a:buftabs
    let altstat .= buftabs#statusline()
    let usealt = 1
  endif

  if usealt
    return altstat
  endif

  " column
  "   this might seem a bit complicated but all it amounts to is
  "   a calculation to see how much padding should be used for the
  "   column number, so that it lines up nicely with the line numbers

  "   an expression is needed because expressions are evaluated within
  "   the context of the window for which the statusline is being prepared
  "   this is crucial because the line and virtcol functions otherwise
  "   operate on the currently focused window

  function! Column()
    let vc = virtcol('.')
    let ruler_width = max([strlen(line('$')), (&numberwidth - 1)]) + &l:foldcolumn
    let column_width = strlen(vc)
    let padding = ruler_width - column_width
    let column = ''

    if padding <= 0
      let column .= vc
    else
      " + 1 because for some reason vim eats one of the spaces
      let column .= repeat(' ', padding + 1) . vc
    endif

    return column
  endfunction

  let stat .= '%#statusColNr#'
  let stat .= '%{Column()}'
  let stat .= '%*'
  if active
    let stat .= "%#statusColNrPowerline#"
  else
    let stat .= "%#statusColNcPowerline#"
  endif
  let stat .= "\ue0b0%*"

  " file name
  let stat .= Color(active, 'statusFileName', ' %<%f')

  " file modified
  let modified = getbufvar(bufnum, '&modified')
  let stat .= Color(active, 'statusFlag', modified ? ' +' : '')

  " read only
  let readonly = getbufvar(bufnum, '&readonly')
  let stat .= Color(active, 'statusFlag', readonly ? ' ‼' : '')

  " paste
  if active
    if !getwinvar(a:winnum, '&spell')
      let stat .= Color(active, 'statusFlag', ' ⌀')
    endif

    if getwinvar(a:winnum, '&paste')
      let stat .= Color(active, 'statusFlag', ' 📋')
    endif
  endif

  " right side
  let stat .= '%='

  " git branch
  if exists('*fugitive#head')
    let head = fugitive#head()

    if empty(head) && exists('*fugitive#detect') && !exists('b:git_dir')
      call fugitive#detect(getcwd())
      let head = fugitive#head()
    endif
  endif

  if !empty(head)
    let stat .= Color(active, 'statusBranch', " \ue0a0 " . head) . ' '
  endif

  return stat
endfunction

function! s:RefreshStatus(buftabs)
  for nr in range(1, winnr('$'))
    call setwinvar(nr, '&statusline', '%!Status(' . nr . ',' . a:buftabs . ')')
  endfor
endfunction

command! RefreshStatus :call <SID>RefreshStatus(0)

augroup status
  autocmd!
  " Update the status line a bunch of times
  autocmd VimEnter,VimLeave,WinEnter,WinLeave,BufWinLeave * call <SID>RefreshStatus(0)
  " On occasion, simply refresh the status line
  autocmd CursorHold * :RefreshStatus
  " When to show the buffer list instead of the filename
  "autocmd BufNew,BufEnter,BufWritePost,BufDelete,InsertLeave,BufWinEnter * call <SID>RefreshStatus(1)
augroup END
