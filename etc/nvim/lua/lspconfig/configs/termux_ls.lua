local util = require 'lspconfig.util'

return {
  default_config = {
    cmd = { 'termux-language-server' },
    filetypes = {
      -- Android Termux
      'sh.build',              -- build.sh
      'sh.subpackage',         -- *.subpackage.sh
      -- ArchLinux/Windows Msys2
      'sh.PKGBUILD',           -- PKGBUILD
      'sh.install',            -- *.install
      'sh.makepkg.conf',       -- makepkg.conf
      -- Gentoo
      'sh.ebuild',             -- *.ebuild
      'sh.eclass',             -- *.eclass
      'sh.make.conf',          -- /etc/make.conf, /etc/portage/make.conf
      'sh.color.map',          -- /etc/portage/color.map
      -- Zsh
      'sh.mdd',                -- *.mdd
    },
    root_dir = function(fname)
      local gentoo_repo = util.root_pattern 'profiles/repo_name' (fname)
      -- TODO: Root detection for Termux and ArchLinux?
      return gentoo_repo or util.find_git_ancestor(fname)
    end,
    single_file_support = true,
  },
  docs = {
    description = [[
Termux is a language server for some specific bash scripts.

You can install termux-language-server using mason or follow the instructions here: https://termux-language-server.readthedocs.io/en/latest/resources/install.html
]],
  },
}
