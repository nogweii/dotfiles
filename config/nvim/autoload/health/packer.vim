function! health#packer#check() abort
  call health#report_start('is packer installed correctly?')

  if luaeval('_G.packer_exists')
    call health#report_ok("packer.nvim package was found")
  else
    call health#report_error("Plugins are not installed!", ['Run zsh function `nvim-packs`', 'Check directory ' . stdpath("data") . '/site/pack/packer/start/packer.nvim'])
  endif
endfunction
