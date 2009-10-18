Thread.abort_on_exception = true # Saves a lot of guess work if a bg thread fails

# Load various things...
%w{wirble open-uri etc guessmethod}.each { |gem| require gem }

if defined? Wirble
	Wirble.init
	Wirble.colorize
end

$START_HISTORY = Readline::HISTORY.size

# Returns a list of all the methods of an obj.
#
# returns: Array
def ms(objl, parents=true)
	if obj
		if (obj.respond_to?(:new) && obj.class === Class)
			(obj.methods + obj.private_methods + obj.public_methods + obj.protected_methods + obj.instance_methods).uniq.sort
		else
			(obj.methods + obj.private_methods + obj.public_methods + obj.protected_methods).uniq.sort
		end
	else
		(methods + private_methods + public_methods + protected_methods + instance_methods).uniq.sort
	end
end

module Kernel
    def ri(arg)
      puts `fri "#{arg}"`
    end
end
 
class Object
  def puts_ri_documentation_for(obj, meth)
    case self
    when Module
      candidates = ancestors.map{|klass| "#{klass}::#{meth}"}
      candidates.concat(class << self; ancestors end.map{|k| "#{k}##{meth}"})
    else
      candidates = self.class.ancestors.map{|klass| "#{klass}##{meth}"}
    end
    candidates.each do |candidate|
      #puts "TRYING #{candidate}"
      desc = `qri '#{candidate}'`
      unless desc.chomp == "nil"
      # uncomment to use ri (and some patience)
      #desc = `ri -T '#{candidate}' 2>/dev/null`
      #unless desc.empty?
        puts desc
        return true
      end
    end
    false
  end
  private :puts_ri_documentation_for
 
  def method_missing(meth, *args, &block)
    if md = /ri_(.*)/.match(meth.to_s) or md = /(.*)_ri/.match(meth.to_s)
      unless puts_ri_documentation_for(self,md[1])
        "Ri doesn't know about ##{meth}"
      end
    else
      super
    end
  end
 
  def ri_(meth)
    unless puts_ri_documentation_for(self,meth.to_s)
      "Ri doesn't know about ##{meth}"
    end
  end
end

def ls
	puts Dir["*"]
end
def pwd
	puts Dir.pwd
end
 
RICompletionProc = proc{|input|
  bind = IRB.conf[:MAIN_CONTEXT].workspace.binding
  case input
  when /(\s*(.*)\.ri_)(.*)/
    pre = $1
    receiver = $2
    meth = $3 ? /\A#{Regexp.quote($3)}/ : /./ #}
    begin
      candidates = eval("#{receiver}.methods", bind).map do |m|
        case m
        when /[A-Za-z_]/; m
        else # needs escaping
          %{"#{m}"}
        end
      end
      candidates = candidates.grep(meth)
      candidates.map{|s| pre + s }
    rescue Exception
      candidates = []
    end
  when /([A-Z]\w+)#(\w*)/ #}
    klass = $1
    meth = $2 ? /\A#{Regexp.quote($2)}/ : /./
    candidates = eval("#{klass}.instance_methods(false)", bind)
    candidates = candidates.grep(meth)
    candidates.map{|s| "'" + klass + '#' + s + "'"}
  else
    IRB::InputCompletor::CompletionProc.call(input)
  end
}
#Readline.basic_word_break_characters= " \t\n\"\\'`><=;|&{("
Readline.basic_word_break_characters= " \t\n\\><=;|&"
Readline.completion_proc = RICompletionProc

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
alias find_libs find_lib
