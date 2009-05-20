===============================
My configuration files, et all.
===============================

Application configurations you can find here:

 * zsh
 * mutt
 * irb
 * vim
 * *more!*

I also have a lazy symlink script here as well, because I'm lazy.

zsh Tweaks
==========

Included in my zsh configs are a few features I want to highlight here:

 * *Missing Features* - Some aspects of the configuration are surrounded by
   checks to not annoy me as much when I'm on a computer that doesn't have all
   the software referenced in the configuration. Also, a message is displayed
   listing those features which have been disabled. I don't like nasty surprises.

 * *Shell Templates* - Sample here: http://gist.github.com/104054 ; implemented
   with some checks as a function 'ztmpl' in zsh/functions. This takes a file
   and runs it through zsh with prompt and variable expansion. See
   gitconfig.tmpl for a sample input file.
