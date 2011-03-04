$:.unshift(File.dirname(__FILE__) + '/../lib')

require 'erlectricity'
require 'benchmark'

data = [:ok, [:foo, :bar, [99, "bottles", "of", "beer", 3.14], [true, false]]]
bert = Erlectricity::Encoder.encode(data)

p bert

Benchmark.bm do|b|
  b.report("Decoder") do
    100_000.times { Erl::Decoder.decode(bert) }
  end
end

#               user        system     total    real
# C Decoder      0.400000   0.000000   0.400000 (  0.425373)
# Ruby Decoder  30.250000   0.220000  30.470000 ( 32.140890)
#
# C decoder is 75.56x faster than Ruby decoder on this data