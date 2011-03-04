module Erlectricity
  class BooleanCondition < Condition
    def satisfies?(arg)
      [TrueClass, FalseClass].include?(arg.class)
    end

    def binding_for(arg)
      arg
    end
  end
end