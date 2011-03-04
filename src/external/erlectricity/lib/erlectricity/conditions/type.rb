module Erlectricity
  class TypeCondition < Condition
    attr_accessor :type

    def initialize(type)
      self.type = type
    end

    def satisfies?(arg)
      arg.is_a?(self.type)
    end

    def binding_for(arg)
      arg
    end
  end
end