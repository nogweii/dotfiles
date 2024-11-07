-- Additional, more specific sub-filetypes for termux-language-server
vim.filetype.add {
  extension = {
    -- ArchLinux/Windows Msys2
    install = 'sh.install',
    -- Gentoo
    ebuild = 'sh.ebuild',
    eclass = 'sh.eclass',
    -- Zsh
    mdd = 'sh.mdd',
  },
  filename = {
    -- Android Termux
    ['build.sh'] = 'sh.build',
    -- ArchLinux/Windows Msys2
    ['PKGBUILD'] = 'sh.PKGBUILD',
    ['makepkg.conf'] = 'sh.makepkg.conf',
  },
  pattern = {
    -- Android Termux
    ['.*%.subpackage%.sh'] = 'sh.subpackage',
    -- Gentoo
    ['.*/etc/make%.conf'] = 'sh.make.conf',
    ['.*/etc/portage/make%.conf'] = 'sh.make.conf',
    ['.*/etc/portage/color%.map'] = 'sh.color.map',
  },
}
