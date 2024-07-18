#!/usr/bin/ruby

require "dbus"
require "pastel"

class EavesdropRule < DBus::MatchRule
  def to_s
    result = super
    "eavesdrop=true,#{result}"
  end
end

class MonitoringBus < DBus::Connection
  attr_accessor :monitoring_mode
  attr_accessor :monitoring_func

  def new(path)
    super(path)
    @monitoring_mode = false
    @monitoring_func = Proc.new { }
  end

  def process(m)
    if @monitoring_mode
      @monitoring_func.call(m)
    else
      super(m)
    end
  end
end

@pastel = Pastel.new(enabled: $stdout.tty?)

mr = EavesdropRule.new
mr.interface = "org.freedesktop.Notifications"
mr.member = "Notify"
mr.path = "/org/freedesktop/Notifications"

# bus = DBus::SessionBus.instance
bus = MonitoringBus.new(DBus::SessionBus.session_bus_address)
bus.send(:send_hello)

monitoring = bus.service("org.freedesktop.DBus").object("/org/freedesktop/DBus")["org.freedesktop.DBus.Monitoring"]
monitoring.BecomeMonitor([mr.to_s], 0)

bus.monitoring_mode = true
bus.monitoring_func = ->(message) do
  # Skip the NameLost message we receive when becoming a monitor
  return if message.member == "NameLost"

  sender = @pastel.cyan(message.params[0])
  notif_summary = message.params[3]
  notif_body = message.params[4]
  if notif_body.empty?
    notification_text = @pastel.green(notif_summary)
  else
    notification_text = "#{@pastel.green(notif_summary)}: #{@pastel.magenta(notif_body)}"
  end
  puts "#{sender}: #{notification_text}"
end

begin
  puts "Waiting for XDG notifications..."
  main = DBus::Main.new
  main << bus
  main.run
rescue Interrupt => e
  puts "Goodbye!"
end
