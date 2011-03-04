$:.unshift File.join(File.dirname(__FILE__), *%w[../../lib])

require 'rubygems'
require 'erlectricity'

receive do |f|
  f.when([:echo, String]) do |text|
    f.send!([:result, "You said: #{text}"])
    f.receive_loop
  end
end