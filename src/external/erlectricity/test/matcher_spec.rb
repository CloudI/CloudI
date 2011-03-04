require File.dirname(__FILE__) + '/test_helper.rb'

def false_match(matcher, arg)
   matcher.matches?(arg).should == false
end

context "A matcher whose condition is a String (the class object" do
  setup do
    @matcher = Erlectricity::Matcher.new(nil, Erlectricity::TypeCondition.new(String), nil)
  end

  specify "should match any string" do
    @matcher.matches?("foo").should == true
  end

  specify "should not match symbols" do
    @matcher.matches?(:foo).should == false
  end
end

context "A matcher whose condition is Symbol (the class object)" do
  setup do
    @matcher = Erlectricity::Matcher.new(nil, Erlectricity::TypeCondition.new(Symbol), nil)
  end

  specify "should match any symbol" do
    @matcher.matches?(:foo).should == true
    @matcher.matches?(:bar).should == true
    @matcher.matches?(:baz).should == true
  end

  specify "should not match strings" do
    @matcher.matches?("foo").should == false
    @matcher.matches?("bar").should == false
    @matcher.matches?("baz").should == false
  end

  specify "should not match a arrays" do
    @matcher.matches?([:foo]).should == false
    @matcher.matches?([:foo, :bar]).should == false
    @matcher.matches?([:foo, :bar, :baz]).should == false
  end
end

context "a matcher whose condition is a symbol" do
  setup do
    @matcher = Erlectricity::Matcher.new(nil, Erlectricity::StaticCondition.new(:foo), nil)
  end

  specify "should match that symbol" do
    @matcher.matches?(:foo).should == true
  end

  specify "should not match any other symbol" do
    @matcher.matches?(:bar).should == false
    @matcher.matches?(:baz).should == false
  end
end

context "a matcher whose matcher is an array" do

  specify "should match if all of its children match" do
    Erlectricity::Matcher.new(nil, [Erlectricity::StaticCondition.new(:speak), Erlectricity::TypeCondition.new(Object)], nil).matches?([:paste, "haha"]).should == false

    matcher = Erlectricity::Matcher.new(nil, [Erlectricity::StaticCondition.new(:foo), Erlectricity::StaticCondition.new(:bar)], nil)
    matcher.matches?([:foo, :bar]).should == true
  end

  specify "should not match any of its children dont match" do
    matcher = Erlectricity::Matcher.new(nil, [Erlectricity::StaticCondition.new(:foo), Erlectricity::StaticCondition.new(:bar)], nil)
    matcher.matches?([:foo]).should == false
    matcher.matches?([:foo, :bar, :baz]).should == false
    matcher.matches?([:fooo, :barr]).should == false
    matcher.matches?([3, :bar]).should == false
  end

  specify "should not match if arg isn't an array" do
    matcher = Erlectricity::Matcher.new(nil, [Erlectricity::StaticCondition.new(:foo), Erlectricity::StaticCondition.new(:bar)], nil)
    matcher.matches?(:foo).should == false
  end
end