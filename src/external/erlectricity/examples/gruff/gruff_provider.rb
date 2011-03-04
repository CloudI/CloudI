$:.unshift File.join(File.dirname(__FILE__), *%w[../../lib])

require 'erlectricity'
require 'rubygems'
require 'gruff'

receive do |f|
  f.when([:plot, String, Symbol, String]) do |name, style, font|
    graph = Gruff.const_get(style).new
    graph.title = name
    graph.font = font
    graph.legend_font_size = 10

    f.receive do |g|
      g.when([:data, Symbol, Array]) do |name, points|
        graph.data name, points
        g.receive_loop
      end

      g.when([:labels, Erl.hash]) do |label_data|
        graph.labels = label_data
        g.receive_loop
      end

      g.when(:end) { :ok }
    end

    f.send!([:result, graph.to_blob])
    f.receive_loop
  end
end