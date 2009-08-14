# Load various things...
# * rubygems for some of the following
# * utility_belt (and wirble) for IRB tools
# * ostruct for OpenStruct.new
# * open-uri (because sometimes rfuzz suxx)
# * color (gem, not default library! see http://rubyforge.org/projects/color/)
# for color numeric manipulation
# * Etc for accessing /etc/* (Etc.getlogin type stuff)
# * what_methods ("hello".what? == 5 #=> ["length", "size"])
# * stringIO for custom less and more commands
# * irb/completion JIC wirble fails
#$KCODE = "utf-8"
Thread.abort_on_exception = true
require 'rubygems'
#require 'method_args'

def prefer lib
	begin
		require lib
	rescue Exception => e
		puts "#{e.class} loading #{lib}: #{e}"
	end
end

%w{rubygems wirble ostruct open-uri etc stringio pp irb/completion facets/ansicode benchmark}.each { |lib| prefer lib }

if defined? Wirble
	Wirble.init
	Wirble.colorize
end
# I think I live in the shell/Vim to much...
alias echo puts
alias q exit
alias q! exit!

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

# Print the methods of an object with colors and arguments and
# lots of other stuff!
def pm(obj, *options)
	methods = obj.methods
	methods -= Object.methods unless options.include? :more
	filter = options.select {|opt| opt.kind_of? Regexp}.first
	methods = methods.select {|name| name =~ filter} if filter

	data = methods.sort.collect do |name|
		method = obj.method(name)
		if method.arity == 0
			args = "()"
		elsif method.arity > 0
			n = method.arity
			args = "(#{(1..n).collect {|i| "arg#{i}"}.join(", ")})"
		elsif method.arity < 0
			n = -method.arity
			args = "(#{(1..n).collect {|i| "arg#{i}"}.join(", ")}, ...)"
		end
		klass = $1 if method.inspect =~ /Method: (.*?)#/
			[name, args, klass]
	end
	max_name = data.collect {|item| item[0].size}.max
	max_args = data.collect {|item| item[1].size}.max
	data.each do |item|
		print " #{ANSICode.bold item[0].rjust(max_name)}"
		print ANSICode.bold(ANSICode.black(item[1].ljust(max_args)))
		print " #{ANSICode.white item[2]}\n"
	end
	data.size
end

def pmm(obj, *options)
	methods = obj.methods
	methods -= Object.methods unless options.include? :more
	filter = options.select {|opt| opt.kind_of? Regexp}.first
	methods = methods.select {|name| name =~ filter} if filter

	data = methods.sort.collect do |name|
		method = obj.method(name)
		args = "()"
		args = 
		klass = $1 if method.inspect =~ /Method: (.*?)#/
		[name, args, klass]
	end
	max_name = data.collect {|item| item[0].size}.max
	max_args = data.collect {|item| item[1].size}.max
	data.each do |item|
		print " #{ANSICode.bold item[0].rjust(max_name)}"
		print ANSICode.bold(ANSICode.black(item[1].ljust(max_args)))
		print " #{ANSICode.white item[2]}\n"
	end
	data.size
end
