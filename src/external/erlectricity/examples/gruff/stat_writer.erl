-module(stat_writer).
-export([start/2, loop/3]).

start(Title, Fun) ->
  spawn(?MODULE, loop, [Title, Fun, []]).

loop(Title, Fun, []) ->
  Data = accumulate([], Fun()),
  loop(Title, Fun, Data, 0).

loop(Title, Fun, Data, Generation) ->
  receive
    {stop} -> ok
  after 3000 ->
    NewGeneration = Generation + 1,
    NewData = accumulate(Data, Fun()),
    NewChart = gruff:plot(
       list_to_binary([Title, << "- Generation" >>, integer_to_list(NewGeneration)]),
      <<"/Users/scott/Library/Fonts/Arial">>,
      NewData,
      []
    ),
    file:write_file(io_lib:format("~s - ~s.png", [Title, integer_to_list(NewGeneration)]),NewChart),
    loop(Title, Fun, NewData, NewGeneration)
  end.

process_axis({Name, PreviousReadings}, {Name, Reading}) ->
  {Name, [Reading|PreviousReadings]}.

accumulate(Data, []) -> Data;
accumulate([], [{Name, Reading}|Rest]) ->
  Data = [{Name, [Reading]}],
  accumulate(Data, Rest);
accumulate(Data, [{Name, Reading}|Rest]) ->
  MergedData =  case lists:keysearch(Name, 1, Data) of
                  {value, Axis} -> lists:keyreplace(Name, 1, Data, process_axis(Axis, {Name, Reading}));
                  false ->
                    [{Name, [Reading]}|Data]
                end,
  accumulate(MergedData, Rest).