# Nogweii's dotfiles

> It just wouldn't be ~/:house: without these.

## Installation

    rake

You're done.

## Layout

This repo is structured to follow the [.local convention](https://gist.github.com/Earnestly/84cf9670b7e11ae2eac6f753910efebe), to a degree.
I only want a single subdirectory of nesting beneath `~/.local/`, rather than 2 or 3 that would result
in fully honoring the [FHS](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard).

As such, here is the resulting tree:

```
HOME
├─ .local
│ ├─ bin
│ ├─ etc
│ ├─ lib
│ ├─ share
│ ├─ srv
│ ├─ tmp
│ └─ var
│
├─ docs
├─ desktop
└─ media
  ├─ music
  ├─ photos
  └─ videos
```

With the following environment variables used to configure XDG-honoring software
to adhere to it:

| Variable              | Location            |
| --------------------- | ------------------- |
| `XDG_CONFIG_HOME`     | `~/.local/etc`      |
| `XDG_DATA_HOME`       | `~/.local/share`    |
| `XDG_CACHE_HOME`      | `~/.local/cache`    |
| `XDG_STATE_HOME`      | `~/.local/var`      |
| `TMPDIR`              | `~/.local/tmp`      |
| `XDG_DOWNLOAD_DIR`    | `~/.local/tmp`      |
| `XDG_PUBLICSHARE_DIR` | `~/.local/srv`      |
| `XDG_DOCUMENTS_DIR`   | `~/docs`            |
| `XDG_MUSIC_DIR`       | `~/media/music`     |
| `XDG_PICTURES_DIR`    | `~/media/photos`    |
| `XDG_VIDEOS_DIR`      | `~/media/videos`    |
| `XDG_TEMPLATES_DIR`   | `~/.local/share/templates` |
| `XDG_DESKTOP_DIR`     | `~/desktop`         |
| `XDG_BIN_DIR`(*)     | `~/.local/bin`         |

*`*` means it is a non-standard environment variable*

##### Inspirations

Many thanks to the following for inspiration for this layout:

* https://man.archlinux.org/man/file-hierarchy.7
* https://github.com/ayekat/localdir
* https://gist.github.com/Earnestly/84cf9670b7e11ae2eac6f753910efebe
* https://github.com/Earnestly/home
* https://wiki.archlinux.org/title/XDG_Base_Directory
* https://man.archlinux.org/man/user-dirs.dirs.5.en
* https://dirs.dev/
* https://man.archlinux.org/man/environ.7.en
