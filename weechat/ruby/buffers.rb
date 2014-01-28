# Copyright (C) 2013 Kyle Johnson <kyle@vacantminded.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.)

def weechat_init
  Weechat.register 'buffers', 'Kabaka', '0.4', 'MIT',
    'High-Performance Buffers List', '', ''

  @bar_name       = 'ruby-buffers'
  @bar_item_name  = 'ruby-buffers'

  @hide, @collapse, @hotlist  = true, true, true

  @hidden, @collapsed = [],   []

  @config_options = {
    'current_fg' => {
      :desc     => 'Foreground color for the currently selected buffer.',
      :default  => 'black'
    },
    'current_bg' => {
      :desc     => 'Background color for the currently selected buffer.',
      :default  => 'white'
    },
    'default_fg' => {
      :desc     => 'Foreground color for buffers which are not selected and are not in the hotlist.',
      :default  => 'default'
    },
    'default_bg' => {
      :desc     => 'Background color for buffers which are not selected and are not in the hotlist.',
      :default  => 'default'
    },
    'inactive_fg' => {
      :desc     => 'Foreground color for buffers which are not active (not current merged buffer) and not in the hotlist.',
      :default  => 'default'
    },
    'inactive_bg' => {
      :desc     => 'Background color for buffers which are not active (not current merged buffer) and not in the hotlist.',
      :default  => 'default'
    },
    'lag_fg' => {
      :desc     => 'Foreground color for server latency.',
      :default  => 'default'
    },
    'lag_bg' => {
      :desc     => 'Background color for server latency.',
      :default  => 'default'
    },
    'number_fg' => {
      :desc     => 'Foreground color for buffer number.',
      :default  => 'default'
    },
    'number_bg' => {
      :desc     => 'Background color for buffer number.',
      :default  => 'default'
    },
    'lag_minimum' => {
      :desc     => 'Minimum value for which to display server latency. 0 to always display.',
      :default  => '0'
    },
    'hotlist_only' => {
      :desc     => 'Only show buffers which appear in the hotlist (visibility toggled with /buffers toggle or /buffers toggle hotlist).',
      :default  => 'no'
    }
  }

  initialize_config
  read_config

  Weechat.bar_item_new @bar_item_name, 'generate', ''

  Weechat.bar_new @bar_name, '0', '0',
    'root', '', 'left', 'columns_vertical', 'vertical',
    '0', '0', 'default', 'default', 'default', '1', @bar_item_name

  Weechat.bar_item_update @bar_item_name

  Weechat.hook_timer 1000, 0, 0, 'redraw_if_scheduled', ''

  # XXX: callbacks cause hidden buffers list to reset -- oops
  #Weechat.hook_config 'plugins.var.ruby.buffers.hidden',    'read_config', ''
  #Weechat.hook_config 'plugins.var.ruby.buffers.collapsed', 'read_config', ''

  redraw_now_hooks = %w[
    buffer_switch
    buffer_merged
    buffer_unmerged
    buffer_moved
    buffer_renamed
    window_switch
  ]

  redraw_schedule_hooks = %w[
    buffer_closed
    buffer_opened
    buffer_title_changed
    buffer_type_changed
    hotlist_changed
  ]

  redraw_now_hooks.each do |hook|
    Weechat.hook_signal hook, 'generate_callback', ''
  end

  redraw_schedule_hooks.each do |hook|
    Weechat.hook_signal hook, 'schedule_redraw', ''
  end

  Weechat.hook_command 'buffers', 'Modify the buffers bar.',
    'toggle [collapsed|hidden|hotlist] | hide [buffer] | unhide [buffer] | collapse [server] | expand [server]',
    [
      'toggle: toggle visibility of some buffers',
      '  collapsed: collapse or expand server buffers',
      '  hidden: toggle buffers that have been individually hidden',
      '  hotlist: toggle buffers based on hotlist_only setting',
      '  all: output all buffers (default behavior)',
      'hide: hide the current buffer from the buffers list',
      '  buffer: buffer name to hide from the buffers list',
      'unhide: allow the current buffer to be shown on the buffers list',
      '  buffer: buffer name to unhide from the buffers list',
      'collapse: only show the server buffer for the current server',
      '  server: server name to collapse',
      'expand: enable display of all buffers for the current server',
      '  server: server name to expand'
    ].join("\n"),
    [
      'toggle collapsed | hidden | hotlist',
      'hide %(buffers_plugins_names)',
      'unhide %(buffers_plugins_names)',
      'collapse %(irc_servers)',
      'expand %(irc_servers)'
    ].join(' || '),
    'buffers_cmd_callback', ''

  # config name is hardcoded because plugin name is not initialized at this point
  Weechat.hook_config 'plugins.var.ruby.buffers.*', 'buffers_config_callback', ''

  Weechat::WEECHAT_RC_OK
end

# Config hook

def buffers_config_callback data, option, value
  generate_callback

  Weechat::WEECHAT_RC_OK
end

# Commands

def buffers_cmd_callback data, buffer, args
  cmd, param = args.split /\s/, 2
  cmd ||= ""

  case cmd.downcase
  when 'toggle'

    case param
    when nil
      @collapse = (not @collapse)
      @hide     = (not @hide)
      @hotlist  = (not @hotlist)

      generate_callback
    when 'collapsed'
      @collapse = (not @collapse)

      generate_callback
    when 'hidden'
      @hide     = (not @hide)

      generate_callback
    when 'hotlist'
      @hotlist  = (not @hotlist)

      generate_callback
    else
      Weechat::WEECHAT_RC_ERROR
    end

  when 'hide'
    hide buffer, param
  when 'unhide'
    unhide buffer, param
  when 'collapse'
    collapse buffer, param
  when 'expand'
    expand buffer, param
  else
    Weechat::WEECHAT_RC_ERROR
  end
end

def hide buffer, param
  target = buffer_or_param buffer, param

  if target.nil?
    return Weechat::WEECHAT_RC_ERROR
  end

  @hidden << target
  @hidden.uniq!
  save_config

  generate_callback
end

def unhide buffer, param
  target = buffer_or_param buffer, param

  if target.nil?
    return Weechat::WEECHAT_RC_ERROR
  end

  @hidden.delete target
  save_config

  generate_callback
end

def collapse buffer, param
  target = server_or_param buffer, param

  if target.nil?
    return Weechat::WEECHAT_RC_ERROR
  end

  @collapsed << target
  @collapsed.uniq!
  save_config

  generate_callback
end

def expand buffer, param
  target = server_or_param buffer, param

  if target.nil?
    return Weechat::WEECHAT_RC_ERROR
  end

  @collapsed.delete target
  save_config

  generate_callback
end

def buffer_or_param buffer, param
  if param
    if buffer_exists? param
      return param
    end
  else
    return Weechat.buffer_get_string buffer, 'full_name'
  end

  nil
end

def server_or_param buffer, param
  if param
    if server_exists? param
      return param
    end
  else
    return Weechat.buffer_get_string(buffer, 'full_name').split('.')[1]
  end

  nil
end

def buffer_exists? buffer
  plugin, buffer = buffer.split('.', 2)

  not Weechat.buffer_search(plugin, buffer).empty?
end

def server_exists? server
  not Weechat.buffer_search('', "server.#{server}").empty?
end


# Config

def initialize_config
  if Weechat.config_is_set_plugin('collapsed').zero?
    Weechat.config_set_plugin 'collapsed', ''
  end

  if Weechat.config_is_set_plugin('hidden').zero?
    Weechat.config_set_plugin 'hidden', ''
  end

  @config_options.each do |option, data|
    if Weechat.config_is_set_plugin(option).nonzero?
      next
    end

    Weechat.config_set_desc_plugin  option, data[:desc]
    Weechat.config_set_plugin       option, data[:default]
  end
end

def save_config
  Weechat.config_set_plugin 'collapsed', @collapsed.join(',')
  Weechat.config_set_plugin 'hidden',    @hidden.join(',')
end

def read_config *args
  initialize_config

  @collapsed  = Weechat.config_get_plugin('collapsed').split(',')
  @hidden     = Weechat.config_get_plugin('hidden').split(',')

  generate_callback
end

def get_config option
  if Weechat.config_is_set_plugin(option).nonzero?
    return Weechat.config_get_plugin option
  end
  
  unless @config_options.has_key? option
    return nil
  end

  @config_options[option][:default]
end


# Output

def schedule_redraw *args
  @redraw = true

  Weechat::WEECHAT_RC_OK
end

def redraw_if_scheduled *args
  return Weechat::WEECHAT_RC_OK unless @redraw
  
  @redraw = false

  generate_callback
end

# generic hook callback to update buffers bar
def generate_callback *args
  Weechat.bar_item_update @bar_item_name

  Weechat::WEECHAT_RC_OK
end

def generate data, item, window
  output, last_number = [], 0

  hotlist = Weechat.infolist_get 'hotlist', '', ''
  hotlist_data = {}

  # TODO: replace with config variables?
  indentation_amount = 3

  hotlist_only = get_config('hotlist_only').downcase == 'yes'

  until Weechat.infolist_next(hotlist).zero? do
    buffer_name = Weechat.infolist_string hotlist, 'buffer_name'
    color       = Weechat.infolist_string hotlist, 'color'

    hotlist_data[buffer_name] = color
  end

  Weechat.infolist_free hotlist

  fg = get_config 'number_fg'
  bg = get_config 'number_bg'
  
  number_color = get_color fg, bg

  buffers = Weechat.infolist_get 'buffer', '', ''

  until Weechat.infolist_next(buffers).zero? do
    line = []

    current      = Weechat.infolist_integer(buffers, 'current_buffer').nonzero?
    active       = Weechat.infolist_integer(buffers, 'active').nonzero?
    number       = Weechat.infolist_integer buffers, 'number'
    name         = Weechat.infolist_string  buffers, 'short_name'
    buffer_name  = Weechat.infolist_string  buffers, 'name'
    full_name    = Weechat.infolist_string  buffers, 'full_name'
    plugin       = Weechat.infolist_string  buffers, 'plugin_name'

    server = buffer_name.split('.', 2).first

    unless current
      if @collapse and @collapsed.include? server
        next
      end

      if @hide and @hidden.include? full_name
        next
      end

      if @hotlist and hotlist_only and not server == 'server'
        next unless hotlist_data[buffer_name]
      end
    end

    unless number == last_number
      line << number_color
      line << number.to_s.rjust(indentation_amount)

      last_number = number
    else
      line << (' ' * indentation_amount)
    end

    if current
      fg = get_config 'current_fg'
      bg = get_config 'current_bg'
      
      color = get_color fg, bg

    elsif hotlist_data[buffer_name]
      color = get_color *(hotlist_data[buffer_name].split(',', 2))

    elsif active
      fg = get_config 'default_fg'
      bg = get_config 'default_bg'

      color = get_color fg, bg

    else
      fg = get_config 'inactive_fg'
      bg = get_config 'inactive_bg'

      color = get_color fg, bg
    end

    line << color
    
    is_channel = Weechat.info_get('irc_is_channel',
                                  "#{server},#{name}") == '1'

    if is_channel
      display_name = "  #{name}"
    elsif plugin == 'irc' and buffer_name.start_with? 'server.'
      display_name = "#{name} #{get_lag_s name}"
    elsif plugin == 'irc'
      display_name = "  #{name}"
    else
      display_name = name
    end
    
    line << " #{display_name}"

    if @collapse and server == 'server'
      if @collapsed.include? name
         line << " ++"
      end
    end

    output << line.join
  end

  Weechat.infolist_free buffers

  output.join "\n"
end


# output helpers

def get_lag_s server
  min = get_config('lag_minimum').to_f

  server_infolist = Weechat.infolist_get 'irc_server', '', server

  Weechat.infolist_next server_infolist

  lag = Weechat.infolist_integer server_infolist, 'lag'

  Weechat.infolist_free server_infolist

  lag = lag.to_f / 1000

  if min > 0 and lag < min
    return ''
  end
  
  fg = get_config 'lag_fg'
  bg = get_config 'lag_bg'

  lag = sprintf '%.3f', lag

  "#{get_color fg, bg}(#{lag})"
end

def get_color fg = 'default', bg = 'default'
  Weechat.color "#{fg},#{bg}"
end

