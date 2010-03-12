# Saves a lot of guess work if a background thread fails
Thread.abort_on_exception = true

# Load various things...
%w{wirble open-uri etc guessmethod}.each { |gem| require gem }

if defined? Wirble
    Wirble.init
    Wirble.colorize
end

$START_HISTORY = Readline::HISTORY.size

# Finds the location of any libraries passed to it.
def find_lib *args
    libs = args.map do |lib|
        libf = $LOADED_FEATURES.grep(/#{Regexp.escape("#{lib}")}/).first
        File.join($LOAD_PATH[$LOAD_PATH.map do |f|
            File.exists? "#{f}/#{libf}"
        end.index(true)], libf)
    end
    libs.first if libs.size == 1
end
