module Erlectricity
  class StaticCondition < Condition
    attr_accessor :value
    def initialize(value)
      if value.is_a?(Array)
        self.value = value.map do |v|
          Condition.for(v)
        end
      else
        self.value = value
      end
    end

    def satisfies?(arg)
      if value.is_a?(Array)
        return false unless arg.is_a?(Array)
        return false if value.length != arg.length
        value.zip(arg).all? do |l, r|
          l.respond_to?(:satisfies?) ? l.satisfies?(r) : l.eql?(r)
        end
      else
        arg.eql?(value)
      end
    end

    def binding_for(arg)
      if value.is_a?(Array)
        value.zip(arg).map { |l, r| l.binding_for(r) }.compact
      else
        nil
      end
    end
  end
end
