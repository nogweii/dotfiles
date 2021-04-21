" Check various programs I use and generally expect that ALE supports
let s:linters_to_check = {
      \  'ansible': ['ansible_lint'],
      \  'css': ['stylelint'],
      \  'elixir': ['credo'],
      \  'go': ['golangci_lint'],
      \  'html': ['tidy'],
      \  'javascript': ['standard'],
      \  'lua': ['luacheck'],
      \  'python': ['flake8', 'isort'],
      \  'ruby': ['standardrb', 'ruby'],
      \  'sh': ['shellcheck'],
      \  'vim': ['vint'],
      \}

function s:linter_exec_check(language, linter) abort
  " Attempt to load the files from ALE
  exec "runtime! ale_linters/" . a:language . '/' . a:linter . '.vim'
  exec "runtime! autoload/ale/fixers/" . a:linter . '.vim'

  let l:ale_var = a:language . '_' . a:linter . '_executable'
  try
    let l:binary = ale#Var(0, l:ale_var)
  catch 
    call health#report_error("Unrecognized linter and language " . l:ale_var)
    return
  endtry

  let l:bin_found = executable(l:binary)
  if l:bin_found
    call health#report_ok("Linter " . l:binary . " found in $PATH: " . exepath(l:binary))
  else
    call health#report_warn("Can not find linter " . l:binary . " in $PATH")
  endif
endfunction

function! health#linters#check() abort
  for language in keys(s:linters_to_check)
    call health#report_start('checking which ' . language . ' linters are installed')

    for linter in s:linters_to_check[language]
      call s:linter_exec_check(language, linter)
    endfor
  endfor

  call health#report_start('misc other linter commands')

  if executable("hadolint")
    call health#report_ok("Docker linter hadolint found in $PATH: " . exepath('hadolint'))
  else
    call health#report_warn("Can not find `hadolint` in $PATH")
  endif

  if executable("languagetool")
    call health#report_ok("Text linter languagetool found in $PATH: " . exepath('languagetool'))
  else
    call health#report_warn("Can not find `languagetool` in $PATH")
  endif

  if executable("credo")
    call health#report_ok("Elixir linter credo found in $PATH: " . exepath('credo'))
  else
    call health#report_warn("Can not find `credo` in $PATH")
  endif
endfunction
