# Monkey patch AwesomePrint 1.8.0 to fix a warning from Ruby 2.7+
# See https://github.com/awesome-print/awesome_print/pull/373

module AwesomePrint
  module Formatters
    class BaseFormatter
      def indented(&block)
        inspector.increase_indentation(&block)
      end
    end
  end

  class Inspector
    def increase_indentation(&block)
      indentator.indent(&block)
    end
  end
end
