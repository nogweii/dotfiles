--------------------------------
-- Calvin & Hobbes, in SPACE! --
--------------------------------
-- An awesome theme purpose-designed to fit with my background image. Inherits
-- from the default theme, mostly it's icons.

local dofile = dofile
local pcall = pcall

-- Load the default theme
success, theme = pcall(function() return dofile("/usr/share/awesome/themes/default/theme.lua") end)

-- Overwrite & personalize the colors, etc
theme.font          = "Droid Sans 10"

theme.bg_normal     = "#000000"
theme.bg_focus      = "#aaaaaa"
theme.bg_urgent     = "#000000"
theme.bg_minimize   = "#000000"

theme.fg_normal     = "#ffffff"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

-- Remove the border
theme.border_width  = "0"

-- HTML injection, what?
theme.tasklist_bg_focus = "#111111"
theme.tasklist_fg_focus = "#ffc700' underline='single"

theme.tooltip_fg_color = "#ffffff"
theme.tooltip_bg_color = "#000000"

-- Complete list of colors that can be defined:
--
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]

-- Set a random background
theme.wallpaper_cmd = { "awsetbg -r "..os.getenv("XDG_CONFIG_HOME").."/awesome/backgrounds/" }

return theme
