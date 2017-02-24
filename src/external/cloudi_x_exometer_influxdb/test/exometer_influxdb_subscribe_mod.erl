-module(exometer_influxdb_subscribe_mod).
-compile([export_all]).

subscribe([metric, test], histogram) ->
    Extra = [{tags, [{type, {from_name, 2}}]}],
    [{[metric, test], min, 1000, Extra},
     {[metric, test], max, 1000, Extra},
     {[metric, test], median, 1000, Extra}];
subscribe([metric, test1], histogram) ->
    Extra = [{tags, [{type, {from_name, 2}}]}],
    [{[metric, test1], max, 1000, Extra, false},
     {[metric, test1], median, 1000, Extra}];
subscribe(_, _) -> [].
