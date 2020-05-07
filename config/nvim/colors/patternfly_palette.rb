#!/usr/bin/ruby

require 'date'
require 'tco'
require 'erb'

# Color palette from Patternfly v4
# https://www.patternfly.org/v4/design-guidelines/styles/colors
$pf_colors = {
    black_100:       "#FAFAFA",
    black_150:       "#F5F5F5",
    black_200:       "#EDEDED",
    black_300:       "#D2D2D2",
    black_400:       "#B8BBBE",
    black_500:       "#8A8D90",
    black_600:       "#737679",
    black_700:       "#4F5255",
    black_800:       "#3C3F42",
    black_850:       "#212427",
    black_900:       "#151515",
    black_1000:      "#030303",
    blue_50:         "#DEF3FF",
    blue_100:        "#BEE1F4",
    blue_200:        "#73BCF7",
    blue_300:        "#2B9AF3",
    blue_400:        "#0066CC",
    blue_500:        "#004080",
    blue_600:        "#004368",
    blue_700:        "#002235",
    cyan_100:        "#A2D9D9",
    cyan_200:        "#73C5C5",
    cyan_300:        "#009596",
    cyan_400:        "#005F60",
    cyan_500:        "#003737",
    cyan_600:        "#003D44",
    cyan_700:        "#001F22",
    gold_100:        "#F9E0A2",
    gold_200:        "#F6D173",
    gold_300:        "#F4C145",
    gold_400:        "#F0AB00",
    gold_500:        "#C58C00",
    gold_600:        "#795600",
    gold_700:        "#3D2C00",
    green_100:       "#BDE5B8",
    green_200:       "#95D58E",
    green_300:       "#6EC664",
    green_400:       "#5BA352",
    green_500:       "#467F40",
    green_600:       "#1E4F18",
    green_700:       "#0F280D",
    light_blue_100:  "#BEEDF9",
    light_blue_200:  "#7CDBF3",
    light_blue_300:  "#35CAED",
    light_blue_400:  "#00B9E4",
    light_blue_500:  "#008BAD",
    light_blue_600:  "#005C73",
    light_blue_700:  "#002D39",
    light_green_100: "#E4F5BC",
    light_green_200: "#C8EB79",
    light_green_300: "#ACE12E",
    light_green_400: "#92D400",
    light_green_500: "#6CA100",
    light_green_600: "#486B00",
    light_green_700: "#253600",
    orange_100:      "#F4B678",
    orange_200:      "#EF9234",
    orange_300:      "#EC7A08",
    orange_400:      "#C46100",
    orange_500:      "#8F4700",
    orange_600:      "#773D00",
    orange_700:      "#3B1F00",
    purple_100:      "#CBC1FF",
    purple_200:      "#B2A3FF",
    purple_300:      "#A18FFF",
    purple_400:      "#8476D1",
    purple_500:      "#6753AC",
    purple_600:      "#40199A",
    purple_700:      "#1F0066",
    red_100:         "#C9190B",
    red_200:         "#A30000",
    red_300:         "#7D1007",
    red_400:         "#470000",
    red_500:         "#2C0000",
    white:           "#FFFFFF",
}

if __FILE__ == $0

    puts "PatternFly palette, and approximations in the 256 color palette"

    @colouring = Tco::Colouring.new(Tco::Config.new([]))

    # moneky patch so that isatty always return true, forcing color output
    def STDOUT.isatty; true; end

    $pf_colors.each do |color_name, target_color|

        colour_instance = @colouring.get_colour_instance target_color
        two_fifty_six_color_index = @colouring.palette.match_colour colour_instance
        fg = @colouring.get_best_font_colour colour_instance
        bg = @colouring.palette.colours[two_fifty_six_color_index]

        stripe_size = 15

        puts "\n#{color_name.to_s.gsub(/_/, ' ')}"
        print " Official color:    "
        print "\x1b[48;2;#{colour_instance.rgb[0]};#{colour_instance.rgb[1]};#{colour_instance.rgb[2]}m"
        print " "*stripe_size
        print "\x1b[0m"
        print "  #{target_color.downcase}"
        print "\n"

        print "Approx 256 palette: "
        print Tco.colour fg, bg, " "*stripe_size
        print "\x1b[0m"
        puts "  #{bg}, #{two_fifty_six_color_index}"

    end
end
