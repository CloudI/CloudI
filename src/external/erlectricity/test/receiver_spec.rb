require File.dirname(__FILE__) + '/test_helper.rb'

def simple_receiver_and_port(*terms, &block)
  port = FakePort.new(*terms)
  receiver = if block
      Erlectricity::Receiver.new(port, &block)
    else
      Erlectricity::Receiver.new(port) do |f|
        f.when Erl.any do
          :matched
        end
      end
    end
end

context "When a receiver is passed a message that matches two match blocks it" do
  setup do
    @port = FakePort.new([:foo, :foo])
    @receiver = Erlectricity::Receiver.new(@port) do |f|
      f.when([:foo, :foo]) do
        :first
      end

      f.when([:foo, Erl.any]) do
        :second
      end
    end
  end

  specify "should run the first matching receiver's block" do
    @receiver.run.should == :first
  end
end

context "A receiver" do
  specify "should return the result of the match block when finished" do
    simple_receiver_and_port(:foo).run.should == :matched
    simple_receiver_and_port(:bar).run.should == :matched
    simple_receiver_and_port(:bar, :baz).run.should == :matched
  end

  specify "should process another message if the matched block returns the results of receive_loop" do
    recv = simple_receiver_and_port(:foo, :bar, :baz) do |f|
      f.when(:bar) {  }
      f.when(Erl.any) { f.receive_loop }
    end

    recv.run
    recv.port.terms.should == [:baz]
  end

  specify "should properly nest" do
    @port = FakePort.new(:foo, :bar, :baz)
    @receiver = Erlectricity::Receiver.new(@port) do |f|
      f.when(:foo) do
        f.receive do |g|
          g.when(:bar){ :ok }
        end
        f.receive_loop
      end

      f.when(:baz) do
        :done
      end
    end

    @receiver.run.should == :done
    @port.terms.should == []
  end

  specify "should queue up skipped results and restore them when a match happens" do
    @port = FakePort.new(:foo, :baz, :bar)
    @receiver = Erlectricity::Receiver.new(@port) do |f|
      f.when(:foo) do
        f.receive do |g|
          g.when(:bar){ :ok }
        end
        f.receive_loop
      end

      f.when(:baz) do
        :done
      end
    end

    @receiver.run.should == :done
    @port.terms.should == []
  end

  specify "should expose bindings to the matched block" do
    @port = FakePort.new(:foo, :bar, :baz)
    results = []
    @receiver = Erlectricity::Receiver.new(@port) do |f|
      f.when(Erl.atom) do |bindinated|
        results << bindinated
        f.receive_loop
      end
    end

    @receiver.run.should == nil
    results.should == [:foo, :bar, :baz]
  end
end