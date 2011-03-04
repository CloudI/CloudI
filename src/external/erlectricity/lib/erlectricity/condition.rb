module Erlectricity
  class Condition
    def self.for(a)
      case a
        when Condition then a
        when Class then TypeCondition.new(a)
        else StaticCondition.new(a)
      end
    end

    def initialize
    end

    def binding_for(arg)
      nil
    end

    def satisfies?(arg)
      false
    end

    alias === satisfies?
  end

  module Conditions
    def atom
      TypeCondition.new(Symbol)
    end

    def any
      TypeCondition.new(Object)
    end

    def number
      TypeCondition.new(Fixnum)
    end

    def pid
      TypeCondition.new(Erlectricity::Pid)
    end

    def ref
      TypeCondition.new(Erlectricity::NewReference)
    end

    def string
      TypeCondition.new(String)
    end

    def list
      TypeCondition.new(Array)
    end

    def hash
      HashCondition.new()
    end

    def boolean
      BooleanCondition.new()
    end
  end

  extend Conditions
end

Any = Object