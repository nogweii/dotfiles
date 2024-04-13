function! health#linters#check() abort
  call health#report_start('various linter commands')

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
