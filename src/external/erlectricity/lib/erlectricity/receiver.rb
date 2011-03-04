module Erlectricity
  class Receiver
    attr_accessor :port
    attr_accessor :parent
    attr_accessor :matchers

    RECEIVE_LOOP = Object.new
    NO_MATCH = Object.new

    def initialize(port, parent = nil, &block)
      @port = port
      @parent = parent
      @matchers = []
      block.call(self) if block
    end

    def process(arg)
      matcher = @matchers.find { |r| r.matches?(arg) }

      if matcher
        port.restore_skipped
        matcher.run(arg)
      else
        NO_MATCH
      end
    end

    def when(arg, &block)
      condition = Condition.for(arg)
      @matchers << Matcher.new(self, condition, block)
    end

    def run
      loop do
        msg = port.receive
        return if msg.nil?

        case result = process(msg)
          when RECEIVE_LOOP then next
          when NO_MATCH
            port.skipped << msg
            next
          else
            break result
        end
      end
    end

    def receive(&block)
      Receiver.new(port, self, &block).run
    end

    def receive_loop
      RECEIVE_LOOP
    end

    def send!(term)
      port.send(term)
    end
  end
end

module Kernel
  def receive(input = nil, output = nil, &block)
    input ||= IO.new(3)
    output ||= IO.new(4)
    Erlectricity::Receiver.new(Erlectricity::Port.new(input, output), nil, &block).run
  end
end