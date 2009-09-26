MODULE
	rbdict

MODULE SUMMARY
       Key-Value Dictionary as a Red-Black Tree

DESCRIPTION

	Rbdict implements a Key - Value dictionary. An rbdict is a
	representation of a dictionary, where a red-black tree is used
	to store the keys and values.

	This module provides exactly the same interface as the module
	dict but with a defined representation. One difference is that
	while dict considers two keys as different if they do not
	match (=:=), this module considers two keys as different if
	and only if they do not compare equal (==).

DATA TYPES

	rb_dictionary()
	  as returned by new/0

EXPORTS

append(Key, Value, Rbdict1) -> Rbdict2

	Types:

	Key = Value = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	This function appends a new Value to the current list of
	values associated with Key. An exception is generated if the
	initial value associated with Key is not a list of values.

append_list(Key, ValList, Rbdict1) -> Rbdict2

	Types:

	ValList = [Value]
	Key = Value = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	This function appends a list of values ValList to the current
	list of values associated with Key. An exception is generated
	if the initial value associated with Key is not a list of
	values.

erase(Key, Rbdict1) -> Rbdict2

	Types:

	Key = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	This function erases all items with a given key from a
	dictionary.

fetch(Key, Rbdict) -> Value

	Types:

	Key = Value = term()
	Rbdict = rb_dictionary()

	This function returns the value associated with Key in the
	dictionary Rbdict. fetch assumes that the Key is present in
	the dictionary and an exception is generated if Key is not in
	the dictionary.

fetch_keys(Rbdict) -> Keys

	Types:

	Rbdict = rb_dictionary()
	Keys = [term()]

	This function returns a list of all keys in the dictionary.

filter(Pred, Rbdict1) -> Rbdict2

	Types:

	Pred = fun(Key, Value) -> bool()
	 Key = Value = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	Rbdict2 is a dictionary of all keys and values in Rbdict1 for
	which Pred(Key, Value) is true.

find(Key, Rbdict) -> {ok, Value} | error

	Types:

	Key = Value = term()
	Rbdict = rb_dictionary()

	This function searches for a key in a dictionary. Returns {ok,
	Value} where Value is the value associated with Key, or error
	if the key is not present in the dictionary.

fold(Fun, Acc0, Rbdict) -> Acc1

	Types:

	Fun = fun(Key, Value, AccIn) -> AccOut
	 Key = Value = term()
	Acc0 = Acc1 = AccIn = AccOut = term()
	Rbdict = rb_dictionary()

	Calls Fun on successive keys and values of Rbdict together
	with an extra argument Acc (short for accumulator). Fun must
	return a new accumulator which is passed to the next
	call. Acc0 is returned if the list is empty. The evaluation
	order is undefined.

from_list(List) -> Rbdict

	Types:

	List = [{Key, Value}]
	Rbdict = rb_dictionary()

	This function converts the key/value list List to a
	dictionary.

is_key(Key, Rbdict) -> bool()

	Types:

	Key = term()
	Rbdict = rb_dictionary()

	This function tests if Key is contained in the dictionary
	Rbdict.

map(Fun, Rbdict1) -> Rbdict2

	Types:

	Fun = fun(Key, Value1) -> Value2
	 Key = Value1 = Value2 = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	map calls Func on successive keys and values of Rbdict to
	return a new value for each key. The evaluation order is
	undefined.

merge(Fun, Rbdict1, Rbdict2) -> Rbdict3

	Types:

	Fun = fun(Key, Value1, Value2) -> Value
	 Key = Value1 = Value2 = Value3 = term()
	Rbdict1 = Rbdict2 = Rbdict3 = rb_dictionary()

	merge merges two dictionaries, Rbdict1 and Rbdict2, to create
	a new dictionary. All the Key - Value pairs from both
	dictionaries are included in the new dictionary. If a key
	occurs in both dictionaries then Fun is called with the key
	and both values to return a new value. merge could be defined
	as:

	merge(Fun, D1, D2) ->
	    fold(fun (K, V1, D) ->
			 update(K, fun (V2) -> Fun(K, V1, V2) end, V1, D)
		 end, D2, D1).

	but is faster.

new() -> rb_dictionary()

	This function creates a new dictionary.

size(Rbdict) -> int()

	Types:

	Rbdict = rb_dictionary()

	Returns the number of elements in an Rbdict.

store(Key, Value, Rbdict1) -> Rbdict2

	Types:

	Key = Value = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	This function stores a Key - Value pair in a dictionary. If
	the Key already exists in Rbdict1, the associated value is
	replaced by Value.

to_list(Rbdict) -> List

	Types:

	Rbdict = rb_dictionary()
	List = [{Key, Value}]

	This function converts the dictionary to a list
	representation.

update(Key, Fun, Rbdict1) -> Rbdict2

	Types:

	Key = term()
	Fun = fun(Value1) -> Value2
	 Value1 = Value2 = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	Update the a value in a dictionary by calling Fun on the value
	to get a new value. An exception is generated if Key is not
	present in the dictionary.

update(Key, Fun, Initial, Rbdict1) -> Rbdict2

	Types:

	Key = Initial = term()
	Fun = fun(Value1) -> Value2
	 Value1 = Value2 = term()
	Rbdict1 = Rbdict2 = rb_dictionary()

	Update the a value in a dictionary by calling Fun on the value
	to get a new value. If Key is not present in the dictionary
	then Initial will be stored as the first value. For example
	append/3 could be defined as:

	append(Key, Val, D) ->
	    update(Key, fun (Old) -> Old ++ [Val] end, [Val], D).

update_counter(Key, Increment, Rbdict1) -> Rbdict2

	Types:

	Key = term()
	Increment = number()
	Rbdict1 = Rbdict2 = rb_dictionary()

	Add Increment to the value associated with Key and store this
	value. If Key is not present in the dictionary then Increment
	will be stored as the first value.

	This could be defined as:

	update_counter(Key, Incr, D) ->
	    update(Key, fun (Old) -> Old + Incr end, Incr, D).

	but is faster.

Notes

	The functions append and append_list are included so we can
	store keyed values in a list accumulator. For example:

	> D0 = rbdict:new(),
	  D1 = rbdict:store(files, [], D0),
	  D2 = rbdict:append(files, f1, D1),
	  D3 = rbdict:append(files, f2, D2),
	  D4 = rbdict:append(files, f3, D3),
	  rbdict:fetch(files, D4).
	[f1,f2,f3]    

	This saves the trouble of first fetching a keyed value,
	appending a new value to the list of stored values, and
	storing the result.

	The function fetch should be used if the key is known to be in
	the dictionary, otherwise find.

See Also

	dict(3), orddict(3), gb_trees(3)

AUTHOR
	Robert Virding - rvirding@gmail.com

	Copyright © 2008 Robert Virding
