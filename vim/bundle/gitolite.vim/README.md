gitolite.vim
============

Description
-----------

[Vim][] Syntax highlight and indent scripts for [gitolite][] configuration file
`gitolite.conf`.

Installation
------------

gitolite.vim is included in Vim from 7.3.347 on.  If you have older Vim or you
want the latest version, you have many options.

1.  Pathogen

    The recommended method is to install [pathogen.vim][], and then clone
    gitolite.vim repository (or copy the content) under `~/.vim/bundle`:

        cd ~/.vim/bundle
        git clone git://github.com/tmatilai/gitolite.vim.git

2.  Download a released Vimball (`gitolite.vba.gz`) from
    [github][github_downloads] or [vim.org script page][script_page], open it
    in Vim and call `:source %s`.

3.  You can also create and/or install the Vimball from the source by calling:

        # create the Vimball
        make
        # create and install it
        make install-vba

4. Yet another option is to install the files directly:

        # to $HOME/.vim
        make install
        # or systemwide
        sudo make vimdir=/etc/vim install

Copyright
---------

Copyright (c) 2010-2011 Teemu Matilainen <teemu.matilainen@iki.fi>

License: [Apache 2](http://www.apache.org/licenses/LICENSE-2.0)

[Vim]: http://www.vim.org/
[gitolite]: https://github.com/sitaramc/gitolite
[script_page]: http://www.vim.org/scripts/script.php?script_id=2900
[github_downloads]: https://github.com/tmatilai/gitolite.vim/downloads
[pathogen.vim]: https://github.com/tpope/vim-pathogen
