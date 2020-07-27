" Automatically activate a workspace when launching vim in a project dir under
" ~/code/

let g:code_folder = environ()["HOME"] . '/code'

function AutoWorkspace()
  if ! matchstr(getcwd(), g:code_folder) " String coercion is sad, so we have to invert the logic to prove we are underneath the directory
    if ! filereadable(g:workspace_session_name)
      " No saved workspace file readable, now build it
      let l:parent_dir = fnamemodify(g:workspace_session_name, ':p:h')
      if ! isdirectory(l:parent_dir)
        call mkdir(l:parent_dir, 'p')
      endif
      silent execute("ToggleWorkspace")
    endif
  endif
endfunction

augroup AutoWorkspacing
  au!
  au VimEnter * call AutoWorkspace()
augroup END
