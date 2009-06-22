wool
====
An awesome white'n'clean theme for [awesome][].

  [awesome]: http://awesome.naquadah.org/ "awesome window manager"

Usage
-----
wool requires [Winsome][]. You can install Winsome, and then wool, as follows:

    mkdir -p ~/.config/awesome/themes
    cd ~/.config/awesome
    git clone git://github.com/elliottcable/winsome.git
    cd themes
    git clone git://github.com/elliottcable/wool.git

You'll also need to modify your `rc.lua` with something like the following:

    theme = "wool"
    require("beautiful")
    beautiful.init(awful.util.getdir("config") .. "/themes/" .. theme .. "/theme.lua")

  [winsome]: http://github.com/elliottcable/winsome "An awesome theming library for awesome"

Credits
-------
wool ships with [a desktop image][zen-rock-wip] by [Mandolux][], released under a [Creative Commons][] [BY-NC-ND 2.0][] license.

  [zen-rock-wip]: http://flickr.com/photos/mandolux/407396074/ "Zen/Rock Wip by Mandolux on Flickr"
  [Mandolux]: http://mandolux.com/ "Mandolux's homepage: an awesome photographer. period."
  [Creative Commons]: http://creativecommons.org/ "Share, Remix, Reuse: Creative Commons"
  [BY-NC-ND 2.0]: http://creativecommons.org/licenses/by-nc-nd/2.0/deed.en "Creative Commons Attribution-Noncommercial-No Derivative Works 2.0 Deed"
