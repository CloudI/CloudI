require File.dirname(__FILE__) + '/test_helper.rb'

context "Erlectricity::StaticConditions" do
  specify "should satisfy on the same value" do
    Erlectricity::StaticCondition.new(:foo).satisfies?(:foo).should == true
    Erlectricity::StaticCondition.new([:foo]).satisfies?([:foo]).should == true
    Erlectricity::StaticCondition.new(3).satisfies?(3).should == true
  end

  specify "should not satisfy on different values" do
    Erlectricity::StaticCondition.new(:foo).satisfies?("foo").should == false
    Erlectricity::StaticCondition.new([:foo]).satisfies?(:foo).should == false
    Erlectricity::StaticCondition.new(Object.new).satisfies?(Object.new).should == false
    Erlectricity::StaticCondition.new(3).satisfies?(3.0).should == false
  end

  specify "should not produce any bindings" do
    s = Erlectricity::StaticCondition.new(:foo)
    s.binding_for(:foo).should == nil
  end
end

context "Erlectricity::TypeConditions" do
  specify "should be satisfied when the arg has the same class" do
    Erlectricity::TypeCondition.new(Symbol).satisfies?(:foo).should == true
    Erlectricity::TypeCondition.new(Symbol).satisfies?(:bar).should == true
    Erlectricity::TypeCondition.new(String).satisfies?("foo").should == true
    Erlectricity::TypeCondition.new(String).satisfies?("bar").should == true
    Erlectricity::TypeCondition.new(Array).satisfies?([]).should == true
    Erlectricity::TypeCondition.new(Fixnum).satisfies?(3).should == true
  end

  specify "should be satisfied when the arg is of a descendent class" do
    Erlectricity::TypeCondition.new(Object).satisfies?(:foo).should == true
    Erlectricity::TypeCondition.new(Object).satisfies?("foo").should == true
    Erlectricity::TypeCondition.new(Object).satisfies?(3).should == true
  end

  specify "should not be satisfied when the arg is of a different class" do
    Erlectricity::TypeCondition.new(String).satisfies?(:foo).should == false
    Erlectricity::TypeCondition.new(Symbol).satisfies?("foo").should == false
    Erlectricity::TypeCondition.new(Fixnum).satisfies?(3.0).should == false
  end

  specify "should bind the arg with no transormations" do
    s = Erlectricity::TypeCondition.new(Symbol)
    s.binding_for(:foo).should == :foo
    s.binding_for(:bar).should == :bar
  end
end

context "Erlectricity::HashConditions" do
  specify "should satisfy an args of the form [[key, value], [key, value]]" do
    Erlectricity::HashCondition.new.satisfies?([[:foo, 3], [:bar, Object.new]]).should == true
    Erlectricity::HashCondition.new.satisfies?([[:foo, 3]]).should == true
  end

  specify "should satisfy on empty arrays" do
    Erlectricity::HashCondition.new.satisfies?([]).should == true
  end

  specify "should nat satisfy other args" do
     Erlectricity::HashCondition.new.satisfies?(:foo).should == false
     Erlectricity::HashCondition.new.satisfies?("foo").should == false
     Erlectricity::HashCondition.new.satisfies?(3.0).should == false
  end

  specify "should bind to a Hash" do
    s = Erlectricity::HashCondition.new()
    s.binding_for([[:foo, 3], [:bar, [3,4,5]]]).should == {:foo => 3, :bar => [3,4,5] }
  end
end