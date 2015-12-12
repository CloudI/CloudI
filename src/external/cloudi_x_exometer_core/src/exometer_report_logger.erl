%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% @doc Exometer report collector and logger.
%%
%% This module implements a behavior for collecting reporting data and
%% handling it (logging to disk or ets, printing to tty, etc.)
%%
%% The logger has built-in support for receiving input via UDP, TCP or
%% internal Erlang messaging, as well as a plugin API for custom input
%% handling. Correspondingly, it has support for output to TTY or ets, as
%% well as a plugin API for custom output.
%%
%% An example of how the logger can be used can be found in
%% `test/exometer_test_udp_reporter.erl', which implements a UDP-based
%% reporter as well as an input plugin and an output plugin. This reporter
%% is used by `test/exometer_report_SUITE.erl'.
%%
%% Loggers can be combined, e.g. by creating one logger that receives Erlang
%% messages, and other loggers that receive from different sources, prefix
%% their reports and pass them on to the first logger.
%%
%% <h2>Input plugins</h2>
%%
%% An input plugin is initiated by `Module:logger_init_input(State)', where
%% `State' is whatever was given as a `state' option (default: `undefined').
%% The function must create a process and return `{ok, Pid}'. `Pid' is
%% responsible for setting up whatever input channel is desired, and passes
%% on incoming data to the logger via Erlang messages `{plugin, Pid, Data}'.
%%
%% <h2>Output Chaining</h2>
%%
%% Each incoming data item is passed through the list of output operators.
%% Each output operator is able to modify the data (the `tty' and `ets'
%% operators leave the data unchanged). Output plugins receive the data
%% in `Module:logger_handle_data(Data, State)', which must return
%% `{NewData, NewState}'. The state is private to the plugin, while `NewData'
%% will be passed along to the next output operator.
%%
%% <h2>Flow control</h2>
%%
%% The logger will handle flow control automatically for `udp' and `tcp'
%% inputs. If `{active,once}' or `{active, false}', the logger will trigger
%% `{active, once}' each time it has handled an incoming message.
%% If `{active, N}', it will "refill" the port each time it receives an
%% indication that it has become passive.
%%
%% Input plugins create a process in `Module:logger_init_input/1'. This process
%% can mimick the behavior of Erlang ports by sending a `{plugin_passive, Pid}'
%% message to the logger. The logger will reply with a message,
%% `{plugin_active, N}', where `N' is the value given by the `active' option.
%% @end
-module(exometer_report_logger).

-behaviour(gen_server).

-export([new/1]).
-export([start_link/1]).

-export([info/0,
         info/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(st, {id,
             input,
             output}).

-include_lib("parse_trans/include/exprecs.hrl").
-export_records([tcp, udp, tty, ets, int]).

%% input records
-record(tcp, {socket, port, options = [], active = once}).
-record(udp, {socket, port, options = [], active = true}).
%% output records
-record(tty, {prefix = []}).
-record(ets, {tab}).
%% both input and output
-record(int, {process}).
-record(plugin, {module, mod_state, process, active = once}).

-type proplist() :: [{atom(), any()}].

-type logger_info() :: {id, any()}
                     | {input, proplist()}
                     | {output, proplist()}.

-type plugin_state() :: any().

-callback logger_init_input(any()) ->
    {ok, pid()}.
-callback logger_init_output(any()) ->
    {ok, plugin_state()}.
-callback logger_handle_data(binary(), plugin_state()) ->
    {binary(), plugin_state()}.

-spec new([{id, any()} | {input, list()} | {output, list()}]) -> {ok,pid()}.
%% @doc Create a new logger instance.
%%
%% This function creates a logger process with the given input and output
%% parameters.
%%
%% * `{id, ID}' is mainly for documentation and simplifying identification
%%   of instances returned by {@link info/0}.
%% * `{input, PropList}' specifies what the logger listens to. Only the first
%%   `input' entry is regarded, but the option is mandatory.
%% * `{output, PropList}' specifies what the logger should to with received
%%    data. Multiple `output' entries are allowed, and they will be processed
%%    in the order given.
%%
%% Valid input options:
%%
%% * `{mode, udp | tcp | internal | plugin}' defines the protocol
%% * `{active, false | true | once | N}' provides flow control. Default: `true'.
%% * (mode-specific options)
%%
%% Valid output options:
%%
%% * `{mode, tty | ets | plugin | internal}' defines output types
%% * (output-specific options)
%%
%% Mode-specific options, `udp':
%%
%% * `{port, integer()}' - UDP port number
%% * `{options, list()}' - Options to pass to {@link gen_udp:open/2}
%%
%% Mode-specific options, `tcp':
%%
%% * `{port, integer()}' - TCP port number
%% * `{options, list()}' - Options to pass to {@link gen_tcp:listen/2}
%%
%% Mode-specific options, `tty':
%%
%% * `{prefix, iolist()}' - Prefix string inserted before the data, which is
%%   printed as-is (note that any delimiter would need to be part of the prefix)
%%
%% Mode-specific options, `ets':
%% * `{table, ets:table()}' - Ets table identifier. If not specified, an
%%    ordered-set table will be created by the logger process. The incoming
%%    data will be inserted as `{erlang:now(), Data}'.
%%
%% Mode-specific options, `internal':
%% * `{process, PidOrRegname}' specifies another logger instance, which is to
%%   receive data from this logger (if used in output), or which is allowed
%%   to send to this logger (if used in input). If no process is given for
%%   input, any process can send data (on the form
%%   `{exometer_report_logger, Pid, Data}') to this logger.
%%
%% Mode-specific options, `plugin':
%%
%% * `{module, Module}' - name of callback module
%%    (behaviour: `exometer_report_logger')
%% * `{state, State}' - Passed as initial argument to
%%   `Module:logger_init_input/1' or `Module:logger_init_output/1', depending
%%   on whether the plugin is specified as input or output.
%% @end
new(Options) ->
    supervisor:start_child(exometer_report_logger_sup, [Options]).

-spec start_link(proplist()) -> {ok, pid()}.
%% @doc Start function for logger instance.
%%
%% This function is the start function eventually called as a result from
%% {@link new/1}, but whereas `new/1' creates a supervised instance, this
%% function simply creates the process. It would normally not be used directly.
%% @end
start_link(Options) ->
    ID = exometer_util:get_opt(id, Options, undefined),
    Input = get_input(Options),
    Output = get_output(Options),
    gen_server:start_link(?MODULE, {ID, Input, Output}, []).

-spec info() -> [{pid(), [logger_info()]}].
%% @doc List active logger instances.
%%
%% This function lists the instances started via {@link new/1}, along with their
%% respective settings as nested property lists.
%% @end
info() ->
    [{P, info(P)} || {_, P, _, _} <- supervisor:which_children(
                                       exometer_report_logger_sup)].
-spec info(pid()) -> [logger_info()].
%% @doc Lists the settings of a given logger instance.
info(P) ->
    gen_server:call(P, info).

%% Client-side check
get_input(Opts) ->
    L = exometer_util:get_opt(input, Opts),
    get_input(exometer_util:get_opt(mode, L), L).

get_input(tcp, L) ->
    Port = exometer_util:get_opt(port, L),
    Options = exometer_util:get_opt(options, L, []),
    Active = exometer_util:get_opt(
               active, L, get_opt_active(Options)),
    #tcp{port = Port, options = Options, active = Active};
get_input(udp, L) ->
    Port = exometer_util:get_opt(port, L),
    Options = exometer_util:get_opt(options, L, []),
    Active = exometer_util:get_opt(
               active, L, get_opt_active(Options)),
    #udp{port = Port, options = Options, active = Active};
get_input(internal, L) ->
    P = exometer_util:get_opt(process, L, undefined),
    #int{process = P};
get_input(plugin, L) ->
    Mod = exometer_util:get_opt(module, L),
    St  = exometer_util:get_opt(state, L, undefined),
    Active = exometer_util:get_opt(active, L, true),
    #plugin{module = Mod, mod_state = St, active = Active}.


get_opt_active(Opts) ->
    case lists:keyfind(active, 1, Opts) of
        {_, true} -> true;
        {_, N} when is_integer(N) -> N;
        _ -> once
    end.

%% Client-side check
get_output(Opts) ->
    [get_output(exometer_util:get_opt(mode, O), O) || {output, O} <- Opts].

get_output(tty, O) ->
    Prefix = exometer_util:get_opt(prefix, O, []),
    #tty{prefix = Prefix};
get_output(ets, O) ->
    Tab = exometer_util:get_opt(tab, O, undefined),
    #ets{tab = Tab};
get_output(internal, O) ->
    P = exometer_util:get_opt(process, O),
    #int{process = P};
get_output(plugin, O) ->
    Mod = exometer_util:get_opt(module, O),
    St  = exometer_util:get_opt(state, O, undefined),
    #plugin{module = Mod, mod_state = St}.


%% Gen_server callbacks

%% @private
init({ID, Input, Output}) ->
    NewL = init_input(Input),
    NewO = init_output(Output),
    {ok, #st{id = ID,
             input = NewL,
             output = NewO}}.

%% @private
handle_call(info, _, #st{id = ID, input = I, output = O} = S) ->
    {reply, info_(ID, I, O), S};
handle_call(_Req, _From, St) ->
    {reply, {error, unsupported}, St}.

%% @private
handle_cast({socket, Socket}, #st{input = L} = S) ->
    case L of
        #tcp{active = N} = T ->
            case inet:getopts(Socket, [active]) of
                {ok, [{active, false}]} ->
                    inet:setopts(Socket, [{active, N}]);
                _ ->
                    ok
            end,
            {noreply, S#st{input = T#tcp{socket = Socket}}};
        _ ->
            {noreply, S}
    end;
handle_cast(_Msg, St) ->
    {noreply, St}.

%% @private
handle_info({tcp, Socket, Data}, #st{input = #tcp{socket = Socket,
                                                  active = Active},
                                     output = Out} = S) ->
    handle_data(Data, Out),
    check_active(Socket, Active),
    {noreply, S};
handle_info({udp, Socket, _Host, _Port, Data},
            #st{input = #udp{socket = Socket}, output = Out} = S) ->
    Out1 = handle_data(Data, Out),
    {noreply, S#st{output = Out1}};
handle_info({plugin, Pid, Data}, #st{input = #plugin{process = Pid},
                                     output = Out} = S) ->
    Out1 = handle_data(Data, Out),
    {noreply, S#st{output = Out1}};
handle_info({tcp_passive, Socket}, #st{input = #tcp{socket = Socket,
                                                    active = Active}} = S) ->
    inet:setopts(Socket, Active),
    {noreply, S};
handle_info({udp_passive, Socket}, #st{input = #udp{socket = Socket,
                                                    active = Active}} = S) ->
    inet:setopts(Socket, Active),
    {noreply, S};
handle_info({plugin_passive, Pid}, #st{input = #plugin{process = Pid,
                                                       active = Active}} = S) ->
    Pid ! {plugin_active, Active},
    {noreply, S};
handle_info({?MODULE, P, Data}, #st{input = #int{process = Pl},
                                    output = Out} = S)
  when Pl =:= P; Pl =:= undefined ->
    Out1 = handle_data(Data, Out),
    {noreply, S#st{output = Out1}};
handle_info(_, S) ->
    {noreply, S}.

%% @private
terminate(_, _) ->
    ok.

%% @private
code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% End gen_server callbacks

info_(ID, I, O) ->
    [{id, ID},
     {input, ensure_list(pp(I))},
     {output, ensure_list(pp(O))}].

ensure_list(I) when is_tuple(I) ->
    [I];
ensure_list(I) when is_list(I) ->
    I.

%% Copied from git:uwiger/jobs/src/jobs_info.erl
pp(L) when is_list(L) ->
    [pp(X) || X <- L];
pp(X) ->
    case '#is_record-'(X) of
        true ->
            RecName = element(1,X),
            {RecName, lists:zip(
                        '#info-'(RecName,fields),
                        pp(tl(tuple_to_list(X))))};
        false ->
            if is_tuple(X) ->
                    list_to_tuple(pp(tuple_to_list(X)));
               true ->
                    X
            end
    end.

init_input(#tcp{port = Port,
                options = Opts} = T) ->
    _ = spawn_tcp_acceptor(Port, Opts),
    T;
init_input(#udp{port = Port, options = Opts} = U) ->
    {ok, Socket} = gen_udp:open(Port, Opts),
    U#udp{socket = Socket};
init_input(#plugin{module = Mod, mod_state = St} = P) ->
    case Mod:logger_init_input(St) of
        {ok, Pid} when is_pid(Pid) ->
            P#plugin{process = Pid}
    end;
init_input(#int{} = I) ->
    I.


spawn_tcp_acceptor(Port, Opts) ->
    Parent = self(),
    spawn_link(fun() ->
                       {ok, LSock} = gen_tcp:listen(Port, Opts),
                       {ok, Socket} = gen_tcp:accept(LSock),
                       ok = gen_tcp:controlling_process(Socket, Parent),
                       gen_server:cast(Parent, {socket, Socket})
               end).

init_output(Out) ->
    [init_output_(O) || O <- Out].

init_output_(#tty{} = TTY) -> TTY;
init_output_(#int{} = Int) -> Int;
init_output_(#ets{tab = T} = E) ->
    Tab = case T of
              undefined ->
                  ets:new(?MODULE, [ordered_set]);
              _ ->
                  T
          end,
    E#ets{tab = Tab};
init_output_(#plugin{module = Mod, mod_state = St} = P) ->
    {ok, St1} = Mod:logger_init_output(St),
    P#plugin{mod_state = St1}.

check_active(Socket, once) ->
    inet:setopts(Socket, [{active, once}]);
check_active(_Socket, _) ->
    ok.

handle_data(Data, Out) ->
    handle_data(Data, Out, []).

handle_data(Data, [H|T], Acc) ->
    {Data1, H1} = handle_data_(Data, H),
    handle_data(Data1, T, [H1|Acc]);
handle_data(_, [], Acc) ->
    lists:reverse(Acc).

handle_data_(Data, #tty{prefix = Pfx} = Out) ->
    io:fwrite(iolist_to_binary([Pfx, Data, $\n])),
    {Data, Out};
handle_data_(Data, #ets{tab = T} = Out) ->
    ets:insert(T, {erlang:now(), Data}),
    {Data, Out};
handle_data_(Data, #int{process = P} = Out) ->
    try P ! {?MODULE, self(), Data} catch _:_ -> error end,
    {Data, Out};
handle_data_(Data, #plugin{module = Mod, mod_state = ModSt} = Out) ->
    {Data1, ModSt1} = Mod:logger_handle_data(Data, ModSt),
    {Data1, Out#plugin{mod_state = ModSt1}}.



