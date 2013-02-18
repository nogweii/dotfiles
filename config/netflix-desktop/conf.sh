# netflix-desktop configuration file
#
# 0 = Disable
# 1 = Enable
#

# Specify the installation path.
if [ -n "$XDG_DATA_HOME" ]; then
  NETFLIX_DESKTOP=$XDG_DATA_HOME/netflix-desktop
else
  NETFLIX_DESKTOP=$HOME/.netflix-desktop
fi

# Disable the custom profile used to hide the Firefox UI.
NO_CUSTOM_PROFILE=0

# Disable xset usage. xset prevents DPMS and xscreensaver from activating.
NO_XSET=0

# Disable extended attribute checks.
NO_XATTR=0
