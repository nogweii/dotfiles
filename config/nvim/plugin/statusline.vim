" A custom status line, handling all aspects of its display. Heavily based off
" of @blaenk's own status implementation, described on his blog
" http://www.blaenkdenum.com/posts/a-simpler-vim-statusline/

" Includes integration with a slightly modified version of buftabs.
" Uses custom highlights, all beginning with 'status'. See my personal color
" scheme, devolved. Assumes you have a "nerd font" installed, uses the private
" use unicode section to display file types.

" The list of custom highlight groups are as follows:
"  * statusColNr - Column number, far left including the powerline arrow
"  * statusColNrPowerline - Default color of the status bar, when it's active
"  * statusColNcPowerline - Default color of the status bar, when it's inactive
"  * statusFileName - Color of the file name
"  * statusFlag - Color of various state flags (modified, is paste on, etc)
"  * statusBranch - Git branch indicator (from fugitive)

" Generic file type icons
let s:filetype_to_icon = {
   \  'html'        : "\ue736",
   \  'ruby'        : "\ue791",
   \  'markdown'    : "\ue73e",
   \  'vim'         : "\ue7c5",
   \  'bash'        : "\ue614",
   \  'sh'          : "\ue614",
   \  'zsh'         : "\ue614",
   \  'python'      : "\ue606",
   \  'gitconfig'   : "\ue702",
   \  'gitcommit'   : "\ue702",
   \  'javascript'  : "\ue781",
   \  'css'         : "\ue74a",
   \  'php'         : "\ue608",
   \  'java'        : "\ue738",
   \  'go'          : "\ue724",
   \  'c'           : "\ue61e",
   \  'cpp'         : "\ue61d",
   \  'netrw'       : "\ue5fe",
   \  'haskell'     : "\ue61f",
   \  'coffee'      : "\ue751",
   \  'dockerfile'  : "\ue7b0",
   \  'erlang'      : "\ue7b1",
   \  'xml'         : "\ue618",
   \  'text'        : "\uf0f6",
   \  'sass'        : "\ue74b",
   \  'scss'        : "\ue74b",
   \  'yaml'        : "\uf03a",
   \ }

" Some file names are so well-known there are icons just for them. Let's use
" them! Equal matches only, and case-sensitive!
let s:filename_to_icon = {
   \  'Gulpfile.js'  : "\ue763",
   \  'bower.json'   : "\ue61a",
   \  'package.json' : "\ue71e",
   \  'LICENSE'      : "\uf0e3",
   \ }

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

  " file type icon or plain word
  let file_type = getbufvar(bufnum, '&filetype')
  let file_name = fnamemodify(name, ':t')
  if has_key(s:filename_to_icon, file_name)
    let fileicon = s:filename_to_icon[file_name]
  elseif has_key(s:filetype_to_icon, file_type)
    let fileicon = s:filetype_to_icon[file_type]
  else
    let fileicon = file_type
  end
  let stat .= Color(active, 'statusFileType', ' ' . fileicon)

  " file modified
  let modified = getbufvar(bufnum, '&modified')
  let stat .= Color(active, 'statusFlag', modified ? ' +' : '')

  " read only
  let readonly = getbufvar(bufnum, '&readonly')
  let stat .= Color(active, 'statusFlag', readonly ? ' ‼' : '')

  " paste
  if active
    if !getwinvar(a:winnum, '&spell')
      let stat .= Color(active, 'statusFlag', " \uf0cc")
    endif

    if getwinvar(a:winnum, '&paste')
      let stat .= Color(active, 'statusFlag', ' 📋')
    endif
  endif

  " right side
  let stat .= '%='

  " line format
  let line_ending = getbufvar(bufnum, '&fileformat')
  if line_ending ==# 'dos'
    let stat .= Color(active, 'statusFlag', "\ue62a ")
  elseif line_ending ==# 'mac'
    let stat .= Color(active, 'statusFlag', "mac ")
  endif

  " git branch
  if exists('*fugitive#head')
    let head = fugitive#head()

    if empty(head) && exists('*fugitive#detect') && !exists('b:git_dir')
      call fugitive#detect(getcwd())
      let head = fugitive#head()
    endif

    if !empty(head)
      let stat .= Color(active, 'statusBranch', " \ue725 " . head) . ' '
    endif
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
