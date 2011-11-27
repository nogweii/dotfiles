local awful = awful
local button = button
local tonumber = tonumber
local io = io
local string = string

module("volume")

cardid  = 0
channel = "Master"
widget = awful.widget.progressbar()
widget:set_width(12)
widget:set_height(24)
widget:set_vertical(true)
widget:set_background_color("#1a1a1a")
widget:set_border_color(nil)
widget:set_color("#606060")
widget:set_gradient_colors({ "#1a1a1a", "#999999" })
widget:set_ticks_size(2)
widget:set_ticks_gap(1)
widget:set_ticks(true)

-- command must start with a space!
mixercommand = function (command)
       local fd = io.popen("amixer -c " .. cardid .. command)
       local status = fd:read("*all")
       fd:close()
       local volume = string.match(status, "(%d?%d?%d)%%")
       volume = string.format("% 3d", volume)
       status = string.match(status, "%[(o[^%]]*)%]")
       widget:set_value(tonumber(volume))
       if string.find(status, "on", 1, true) then
              widget:set_background_color("#1a1a1a")
       else
              widget:set_background_color("#cc3333")
       end
end
update = function ()
       mixercommand(" sget " .. channel)
end
up = function ()
       mixercommand(" sset " .. channel .. " 5%+")
end
down = function ()
       mixercommand(" sset " .. channel .. " 5%-")
end
toggle = function ()
       mixercommand(" sset " .. channel .. " toggle")
end
widget.widget.buttons = awful.util.table.join({
       button({ }, 4, up),
       button({ }, 5, down),
       button({ }, 1, toggle)
})
