module Erlectricity
  class Matcher
    attr_accessor :condition, :block
    attr_accessor :receiver

    def initialize(parent, condition, block)
      self.receiver = parent
      @block = block
      @condition = Condition.for(condition)
    end

    def run(arg)
      args = @condition.binding_for(arg)
      block.call(*args)
    end

    def matches?(arg)
      @condition.satisfies?(arg)
    end
  end
end
