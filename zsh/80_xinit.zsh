# X.org related integration and functions.
#
# If there some thing zsh needs or should have related to X initialization, then
# it goes here.

GNOME_KEYRING_SOURCE='/tmp/gnome_keyring_auth.sh'
if [ -f $GNOME_KEYRING_SOURCE ] ; then
    source $GNOME_KEYRING_SOURCE
fi
