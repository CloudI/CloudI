#
#  rerl.rb, for use with erlang's open_port
#  spawn using rerl.sh escript:
#
#    $ ./rerl.sh
#    ./rerl.sh:35: Warning: variable 'Port' is unused
#    [erlang] ruby is alive
#    [erlang]  sending: test
#    [erlang]  sending: {atom,symbol}
#    [erlang]  sending: {number,1}
#    [erlang]  sending: {string,<<"reverse">>}
#    [erlang]  sending: {array,[1,2,3]}
#    [erlang]  sending: {array,[<<"abc">>,<<"cde">>]}
#    [erlang]  sending: {hash,[{key,val}]}
#    [erlang]  sending: {object,{1,{2},3,<<"four">>}}
#    [ ruby ] received: test, nil
#    [ ruby ]  sending: test, nil
#    [erlang] received: test
#    [ ruby ] received: atom, :symbol
#    [ ruby ]  sending: atom, :lobmys
#    [erlang] received: {atom,lobmys}
#    [ ruby ] received: number, 1
#    [ ruby ]  sending: number, 2
#    [ ruby ] received: string, "reverse"
#    [erlang] received: {number,2}
#    [ ruby ]  sending: string, "esrever"
#    [ ruby ] received: array, [1, 2, 3]
#    [erlang] received: {string,<<"esrever">>}
#    [ ruby ]  sending: array, [3, 2, 1]
#    [ ruby ] received: array, ["abc", "cde"]
#    [erlang] received: {array,{3,2,1}}
#    [ ruby ]  sending: array, ["cde", "abc"]
#    [ ruby ] received: hash, {:key=>:val}
#    [erlang] received: {array,{<<"cde">>,<<"abc">>}}
#    [ ruby ]  sending: hash, {:key=>:val, :ruby=>:true}
#    [ ruby ] received: object, [1, [2], 3, "four"]
#    [erlang] received: {hash,{{key,val},{ruby,true}}}
#    [ ruby ]  sending: object, [1, [2], 3, "four"]
#    [erlang] received: {object,{1,{2},3,<<"four">>}}
#

$:.unshift File.join(File.dirname(__FILE__), *%w[../../lib])

require 'rubygems'
require 'erlectricity'

def log arg
  puts arg
  # @f ||= File.open('/tmp/rerl.log', 'w')
  # @f.puts arg
  # @f.flush
end

def debug meth, got = nil, send = nil
  log "[ ruby ] received: #{meth}, #{got.inspect}"
  log "[ ruby ]  sending: #{meth}, #{send.inspect}"
end

receive do |f|
  f.when(:test) do
    debug(:test)
    f.send!(:test)
    f.receive_loop
  end

  f.when([:atom, Symbol]) do |sym|
    debug(:atom, sym, sym.to_s.reverse.to_sym)
    f.send!([:atom, sym.to_s.reverse.to_sym])
    f.receive_loop
  end

  f.when([:bool, Erl.boolean]) do |bool|
    debug(:bool, bool, !bool)
    f.send!([:bool, !bool])
    f.receive_loop
  end

  f.when([:number, Fixnum]) do |num|
    debug(:number, num, num*2)
    f.send!([:number, num*2])
    f.receive_loop
  end

  f.when([:string, String]) do |str|
    debug(:string, str, str.reverse)
    f.send!([:string, str.reverse])
    f.receive_loop
  end

  f.when([:array, Array]) do |arr|
    debug(:array, arr, arr.reverse)
    f.send!([:array, arr.reverse])
    f.receive_loop
  end

  f.when([:hash, Erl.hash]) do |hash|
    newhash = hash.dup
    newhash[:ruby] = :true
    debug(:hash, hash, newhash)
    f.send!([:hash, newhash.to_a])
    f.receive_loop
  end

  f.when([:object, Any]) do |obj|
    debug(:object, obj, obj)
    f.send!([:object, obj])
    f.receive_loop
  end
  
  f.send!(:i_am_alive)
end