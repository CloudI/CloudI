module Erlectricity
  class HashCondition < Condition
    def satisfies?(arg)
      return false unless arg.class == Array
      arg.all? { |x| x.class == Array && x.length == 2 }
    end

    def binding_for(arg)
      flattened = arg.inject([]) { |memo, kv| memo + kv }
      Hash[*flattened]
    end
  end
end