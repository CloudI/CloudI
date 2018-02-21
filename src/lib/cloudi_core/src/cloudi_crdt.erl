%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI CRDT (Conflict-free Replicated Data Type)==
%%% This module provides a POLog CRDT implementation with an Erlang map
%%% data type for use in internal CloudI services.  Usage of the module
%%% handles the replication of Erlang map state between Erlang processes to
%%% provide an eventually consistent data store among internal CloudI service
%%% processes.
%%%
%%% The bootstrap functionality and the clean_vclocks functionality are
%%% not described in the POLog papers and are likely unique to this
%%% implementation.  This additional functionality allows CloudI service
%%% processes that utilize cloudi_crdt to start, restart or
%%% fail (a crash, netsplit, etc.) without affecting other instances of
%%% cloudi_crdt that are configured with the same service name and
%%% manage the same data.
%%%
%%% The only cloudi_crdt function that may be called within
%%% cloudi_service_init/4 is the new function.  A CloudI service that
%%% uses cloudi_crdt should have a destination refresh method that is
%%% immediate.
%%%
%%% The papers related to this implementation of the POLog CRDT are:
%%%
%%% Carlos Baquero, Paulo Sérgio Almeida, Ali Shoker.
%%% Pure Operation-Based Replicated Data Types. 2017.
%%% https://arxiv.org/abs/1710.04469
%%%
%%% Georges Younes, Ali Shoker, Paulo Sérgio Almeida, and Carlos Baquero.
%%% Integration Challenges of Pure Operation-based CRDTs in Redis.
%%% In First Workshop on Programming Models and Languages for
%%% Distributed Computing (PMLDC '16). ACM, New York, NY, USA, Article 7, 2016.
%%% http://haslab.uminho.pt/cbm/files/pmldc-2016-redis-crdts.pdf
%%%
%%% Carlos Baquero, Paulo Sérgio Almeida, and Ali Shoker.
%%% Making operation-based crdts operation-based.
%%% In Proceedings of the First Workshop on Principles and
%%% Practice of Eventual Consistency, page 7. ACM, 2014.
%%% http://haslab.uminho.pt/ashoker/files/opbaseddais14.pdf
%%%
%%% Mattern, Friedemann. "Virtual Time and Global States of
%%% Distributed Systems". Workshop on Parallel and Distributed
%%% Algorithms: pp. 215-226 (1988).
%%% http://homes.cs.washington.edu/~arvind/cs425/doc/mattern89virtual.pdf
%%%
%%% Lamport, Leslie. "Time, clocks, and the ordering of events in a
%%% distributed system". Communications of the ACM. 21 (7): 558–565. (1978)
%%% http://research.microsoft.com/en-us/um/people/lamport/pubs/time-clocks.pdf
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017-2018 Michael Truog <mjtruog at gmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2017-2018 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_crdt).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([assign/4,
         clear/2,
         clear/3,
         decr/3,
         decr/4,
         find/3,
         fold/4,
         get/3,
         handle_info/3,
         handle_request/11,
         incr/3,
         incr/4,
         is_key/3,
         keys/2,
         new/1,
         new/2,
         put/4,
         size/2,
         update/5,
         update/6,
         values/2,
         zero/3]).

-type node_id() :: {node(), cloudi_service:source()}.
-type vclock() :: #{node_id() := non_neg_integer()}.
-type vclocks() :: #{node_id() := vclock()}.
-type seconds() :: 1 .. 4294967.
-type milliseconds() :: 1 .. 4294967295.
-type key() :: any().
-type value() :: any().
-type operation_read() ::
    {find,   Key :: key()} |
    {fold,   F :: fun((Key :: key(), Value :: value(),
                       AccIn :: any()) -> AccOut :: any()),
             AccInit :: any()} |
    {get,    Key :: key()} |
    {is_key, Key :: key()} |
    keys |
    size |
    values.
-type operation_write() ::
    {assign, Key :: key(), Value :: value()} |
    {incr,   Key :: key(), Value :: value()} |
    {decr,   Key :: key(), Value :: value()} |
    {update, Key :: key(), ModuleVersion :: list(),
             Module :: module(), Function :: atom()} |
    {update, Key :: key(), ModuleVersion :: list(),
             Module :: module(), Function :: atom(), Argument1 :: any()} |
    {put,    Key :: key(), Value :: value()} |
    {clear,  Key :: key()} |
    clear_all.

% The POLog is ordered based on the receive order of the operations
% (newest operation (head) and older operations (tail)),
% with redundant operations resolved by the redundancy relation
% (which is data type specific, an Erlang map is used here)
-type polog() :: list({vclock(), operation_write()}).

% To allow the POLog usage to work with CloudI service processes
% appearing and disappearing due to restarts or dynamic service configuration
% changes (e.g., count_process_dynamic or new CloudI service processes),
% it is necessary to have a bootstrap sequence that allows the
% CloudI service process to obtain a consistent state from an existing POLog.
-type polog_mode() ::
    bootstrap |
    normal.

% The POLog data type that contains consistent state (an Erlang map)
-type data() :: #{key() := value()}.

-record(cloudi_crdt,
    {
        service_name_full :: cloudi_service:service_name(),
        clean_vclocks_interval :: seconds(),
        clean_vclocks_failure :: number(),
        queue :: cloudi_queue:state(),
        node_id :: node_id(),
        node_ids :: list(node_id()),
        vclock :: vclock(),
        vclocks = vclocks_new() :: vclocks(),
        polog_mode = bootstrap :: polog_mode(),
        polog = [] :: polog(),
        data = #{} :: data()
    }).

-include("cloudi_service.hrl").
-include("cloudi_logger.hrl").

-define(DEFAULT_SERVICE_NAME,                    "crdt").
-define(DEFAULT_CLEAN_VCLOCKS,                       60). % seconds
        % How often to check for the disappearance of CloudI service
        % processes (due to a process restart, netsplit, etc.).
        % The vclock() is cleaned to allow POLog operations to complete
        % after a CloudI service process has restarted or stopped
        % (in those cases, the old vclock() will never get incremented).
-define(DEFAULT_CLEAN_VCLOCKS_FAILURE,             50.0). % percentage
        % If the number of CloudI service processes is reduced by
        % this amount or greater, then do not clean the vclocks
        % (the problem is assumed to be transient, not permanent).
        % Set this properly to avoid the POLog having split-brain
        % after a net-split occurs.  The percentage should be lower
        % with higher Distributed Erlang node counts
        % (e.g., 2 nodes can use 50.0, 3 nodes can use 33.3, etc.).
-define(DEFAULT_RETRY,                                0). % see below:
        % a retry doesn't count as a failure, until it fails completely
        % (i.e., hit the max retry count or send returns an error)
-define(DEFAULT_RETRY_DELAY,                          0). % milliseconds

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type options() ::
    list({service_name, string()} |
         {clean_vclocks, seconds()} |
         {clean_vclocks_failure, float() | 1..100} |
         {retry, non_neg_integer()} |
         {retry_delay, non_neg_integer()}).
-type state() :: #cloudi_crdt{}.
-export_type([options/0,
              state/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Assign a value iff none exists in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec assign(Dispatcher :: cloudi_service:dispatcher(),
             Key :: key(),
             Value :: value(),
             State :: state()) ->
    state().

assign(Dispatcher, Key, Value, State)
    when is_pid(Dispatcher) ->
    event_local({assign, Key, Value}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec clear(Dispatcher :: cloudi_service:dispatcher(),
            State :: state()) ->
    state().

clear(Dispatcher, State)
    when is_pid(Dispatcher) ->
    event_local(clear_all, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear a key in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec clear(Dispatcher :: cloudi_service:dispatcher(),
            Key :: key(),
            State :: state()) ->
    state().

clear(Dispatcher, Key, State)
    when is_pid(Dispatcher) ->
    event_local({clear, Key}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decrement a numerical value by 1 in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec decr(Dispatcher :: cloudi_service:dispatcher(),
           Key :: key(),
           State :: state()) ->
    state().

decr(Dispatcher, Key, State) ->
    decr(Dispatcher, Key, 1, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decrement a numerical value in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec decr(Dispatcher :: cloudi_service:dispatcher(),
           Key :: key(),
           Value :: number(),
           State :: state()) ->
    state().

decr(Dispatcher, Key, Value, State)
    when is_pid(Dispatcher) ->
    event_local({decr, Key, Value}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec find(Dispatcher :: cloudi_service:dispatcher(),
           Key :: key(),
           State :: state()) ->
    {ok, Value :: value()} |
    error.

find(Dispatcher, Key,
     #cloudi_crdt{data = Data})
    when is_pid(Dispatcher) ->
    read({find, Key}, Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec fold(Dispatcher :: cloudi_service:dispatcher(),
           F :: fun((Key :: key(), Value :: value(),
                     AccIn :: any()) -> AccOut :: any()),
           AccInit :: any(),
           State :: state()) ->
    AccFinal :: any().

fold(Dispatcher, F, AccInit,
     #cloudi_crdt{data = Data})
    when is_pid(Dispatcher) ->
    read({fold, F, AccInit}, Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a value from the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Dispatcher :: cloudi_service:dispatcher(),
          Key :: key(),
          State :: state()) ->
    Value :: value().

get(Dispatcher, Key,
    #cloudi_crdt{data = Data})
    when is_pid(Dispatcher) ->
    read({get, Key}, Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Handle all info messages related to the CloudI CRDT.===
%% Must be called from the `cloudi_service_handle_info/3' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec handle_info(Request :: any(),
                  State :: state(),
                  Dispatcher :: cloudi_service:dispatcher()) ->
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()} |
    {ignored, State :: state()}.

handle_info(cloudi_crdt_clean_vclocks, State, Dispatcher) ->
    {ok, clean_vclocks(State, Dispatcher)};
handle_info(Request, #cloudi_crdt{queue = Queue} = State, Dispatcher) ->
    {Result, QueueNew} = cloudi_queue:handle_info(Request, Queue, Dispatcher),
    StateNew = State#cloudi_crdt{queue = QueueNew},
    case Request of
        #return_async_active{response = {vclock,
                                         NodeIdRemote, VClockRemote}}
        when Result == ok ->
            {ok, event_local_vclock(NodeIdRemote, VClockRemote,
                                    StateNew, Dispatcher)};
        #return_async_active{response = {vclock_updated,
                                         NodeIdRemote, VClockRemote}}
        when Result == ok ->
            {ok, event_local_vclock_updated(NodeIdRemote, VClockRemote,
                                            StateNew)};
        #return_async_active{response = vclock_updated}
        when Result == ok ->
            {ok, StateNew};
        _ ->
            {Result, StateNew}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Handle a CRDT service request.===
%% Must be called from the `cloudi_service_handle_request/11' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec handle_request(Type :: cloudi_service:request_type(),
                     Name :: cloudi_service:service_name(),
                     Pattern :: cloudi_service:service_name_pattern(),
                     RequestInfo :: cloudi_service:request_info(),
                     Request :: cloudi_service:request(),
                     Timeout :: cloudi_service:timeout_value_milliseconds(),
                     Priority :: cloudi_service:priority_value(),
                     TransId :: cloudi_service:trans_id(),
                     Pid :: cloudi_service:source(),
                     State :: state(),
                     Dispatcher :: cloudi_service:dispatcher()) ->
    {ok, StateNew :: state()} |
    {ignored, State :: state()}.

handle_request(Type, ServiceNameFull, ServiceNameFull,
               _RequestInfo, Request, Timeout, _Priority, TransId, Pid,
               #cloudi_crdt{service_name_full = ServiceNameFull,
                            node_id = NodeId,
                            vclock = VClock,
                            vclocks = VClocks,
                            polog_mode = POLogMode,
                            polog = POLog,
                            data = Data} = State,
               Dispatcher) ->
    {Response, StateNew} = case Request of
        {operation, NodeIdRemote, VClockRemote, Operation} ->
            event_remote(NodeIdRemote, VClockRemote, Operation,
                         State, Dispatcher);
        {vclock, NodeIdRemote, VClockRemote} ->
            event_remote_vclock(NodeIdRemote, VClockRemote,
                                State, Dispatcher);
        {vclock_updated, NodeIdRemote, VClockRemote} ->
            event_remote_vclock_updated(NodeIdRemote, VClockRemote, State);
        state ->
            {{state,
              NodeId, VClock, VClocks,
              POLogMode, POLog, Data}, State}
    end,
    ok = cloudi_service:return_nothrow(Dispatcher, Type,
                                       ServiceNameFull, ServiceNameFull,
                                       <<>>, Response, Timeout, TransId, Pid),
    {ok, StateNew};
handle_request(_, _, _, _, _, _, _, _, _, State, _) ->
    {ignored, State}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment a numerical value by 1 in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec incr(Dispatcher :: cloudi_service:dispatcher(),
           Key :: key(),
           State :: state()) ->
    state().

incr(Dispatcher, Key, State) ->
    incr(Dispatcher, Key, 1, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment a numerical value in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec incr(Dispatcher :: cloudi_service:dispatcher(),
           Key :: key(),
           Value :: number(),
           State :: state()) ->
    state().

incr(Dispatcher, Key, Value, State)
    when is_pid(Dispatcher) ->
    event_local({incr, Key, Value}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if a key is in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec is_key(Dispatcher :: cloudi_service:dispatcher(),
             Key :: key(),
             State :: state()) ->
    boolean().

is_key(Dispatcher, Key,
       #cloudi_crdt{data = Data})
    when is_pid(Dispatcher) ->
    read({is_key, Key}, Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all keys in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec keys(Dispatcher :: cloudi_service:dispatcher(),
           State :: state()) ->
    list(key()).

keys(Dispatcher,
     #cloudi_crdt{data = Data})
    when is_pid(Dispatcher) ->
    read(keys, Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher()) ->
    state().

new(Dispatcher) ->
    new(Dispatcher, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Options :: options()) ->
    state().

new(Dispatcher, Options)
    when is_pid(Dispatcher), is_list(Options) ->
    Defaults = [
        {service_name,                  ?DEFAULT_SERVICE_NAME},
        {clean_vclocks,                 ?DEFAULT_CLEAN_VCLOCKS},
        {clean_vclocks_failure,         ?DEFAULT_CLEAN_VCLOCKS_FAILURE},
        {retry,                         ?DEFAULT_RETRY},
        {retry_delay,                   ?DEFAULT_RETRY_DELAY}],
    [ServiceName, CleanIntervalSeconds, CleanFailure,
     Retry, RetryDelay] =
        cloudi_proplists:take_values(Defaults, Options),
    true = is_list(ServiceName) andalso is_integer(hd(ServiceName)),
    Prefix = cloudi_service:prefix(Dispatcher),
    ServiceNameFull = Prefix ++ ServiceName,
    false = cloudi_x_trie:is_pattern(ServiceNameFull),
    true = is_integer(CleanIntervalSeconds) andalso
           (CleanIntervalSeconds >= 1) andalso
           (CleanIntervalSeconds =< 4294967),
    true = is_number(CleanFailure) andalso
           (CleanFailure > 0) andalso (CleanFailure =< 100),
    Service = cloudi_service:self(Dispatcher),
    NodeId = node_id(Service),
    VClock0 = vclock_new(),
    VClockN = VClock0#{NodeId => 0},

    % CloudI CRDT service requests need to be ordered so that a retry
    % that occurs after a failed send does not allow a duplicate of an
    % operation to arrive after the operation has been removed from the POLog
    % (i.e., taken effect in the data type, the Erlang map).
    Queue = cloudi_queue:new([{retry, Retry},
                              {retry_delay, RetryDelay},
                              {ordered, true},
                              {failures_source_die, true}]),

    ok = cloudi_service:subscribe(Dispatcher, ServiceName),
    #cloudi_crdt{service_name_full = ServiceNameFull,
                 clean_vclocks_interval = CleanIntervalSeconds,
                 clean_vclocks_failure = CleanFailure,
                 queue = Queue,
                 node_id = NodeId,
                 node_ids = [NodeId],
                 vclock = VClockN}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Put a value into the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Dispatcher :: cloudi_service:dispatcher(),
          Key :: key(),
          Value :: value(),
          State :: state()) ->
    state().

put(Dispatcher, Key, Value, State)
    when is_pid(Dispatcher) ->
    event_local({put, Key, Value}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the size of the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec size(Dispatcher :: cloudi_service:dispatcher(),
           State :: state()) ->
    non_neg_integer().

size(Dispatcher,
     #cloudi_crdt{data = Data})
    when is_pid(Dispatcher) ->
    read(size, Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value iff it exists in the CloudI CRDT.===
%% Function Module:Function/1 must exist with the same version
%% in every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update(Dispatcher :: cloudi_service:dispatcher(),
             Key :: key(),
             Module :: module(),
             Function :: atom(),
             State :: state()) ->
    state().

update(Dispatcher, Key, Module, Function, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 1),
    event_local({update, Key, ModuleVersion, Module, Function},
                State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value iff it exists in the CloudI CRDT.===
%% Function Module:Function/2 must exist with the same version
%% in every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update(Dispatcher :: cloudi_service:dispatcher(),
             Key :: key(),
             Module :: module(),
             Function :: atom(),
             Argument1 :: any(),
             State :: state()) ->
    state().

update(Dispatcher, Key, Module, Function, Argument1, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 2),
    event_local({update, Key, ModuleVersion, Module, Function, Argument1},
                State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all values in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec values(Dispatcher :: cloudi_service:dispatcher(),
             State :: state()) ->
    list(value()).

values(Dispatcher,
       #cloudi_crdt{data = Data})
    when is_pid(Dispatcher) ->
    read(values, Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Put a zero value in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec zero(Dispatcher :: cloudi_service:dispatcher(),
           Key :: key(),
           State :: state()) ->
    state().

zero(Dispatcher, Key, State) ->
    put(Dispatcher, Key, 0, State).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec node_id(Service :: cloudi_service:source()) ->
    node_id().

node_id(Service)
    when is_pid(Service) ->
    % n.b., The Erlang Term Binary Format for an Erlang pid (PID_EXT)
    % includes the node, so only the Erlang pid would be necessary here
    % if a binary format was used.  The node is added here to make it
    % more obvious for human examination.
    {node(Service), Service}.

-spec update_local_valid(Module :: module(),
                         Function :: atom(),
                         Arity :: non_neg_integer()) ->
    ModuleVersion :: list().

update_local_valid(Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true ->
            % if the module was stripped, an empty list is returned and
            % the module version will be unable to get checked elsewhere.
            cloudi_x_reltool_util:module_version(Module);
        false ->
            ?LOG_ERROR("function ~w:~w/~w does not exist!",
                       [Module, Function, Arity]),
            erlang:exit(badarg)
    end.

-spec update_remote_valid(ModuleVersion :: list(),
                          Module :: module(),
                          Function :: atom(),
                          Arity :: non_neg_integer()) ->
    ok.

update_remote_valid([], Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true ->
            ok;
        false ->
            erlang:exit({crdt_update_module_function_missing,
                         Module, Function, Arity})
    end;
update_remote_valid(ModuleVersion, Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true ->
            case cloudi_x_reltool_util:module_version(Module) of
                ModuleVersion ->
                    ok;
                _ ->
                    erlang:exit({crdt_update_module_version_mismatch, Module})
            end;
        false ->
            erlang:exit({crdt_update_module_function_missing,
                         Module, Function, Arity})
    end.

-spec event_local(Operation :: operation_write(),
                  State :: state(),
                  Dispatcher :: cloudi_service:dispatcher()) ->
    state().

event_local(Operation,
            #cloudi_crdt{service_name_full = ServiceNameFull,
                         queue = Queue,
                         node_id = NodeId,
                         vclock = VClock0,
                         polog = POLog} = State,
            Dispatcher) ->
    % A write operation occurs locally by getting added to the POLog
    % and getting broadcasted to all other CloudI service processes.

    VClock1 = vclock_increment(NodeId, VClock0), % event
    POLogNew = polog_effect(Operation, VClock1, POLog),
    VClockN = vclock_increment(NodeId, VClock1), % send
    {ok, QueueNew} = cloudi_queue:
                     mcast(Dispatcher, ServiceNameFull,
                           {operation, NodeId, VClockN, Operation}, Queue),
    State#cloudi_crdt{queue = QueueNew,
                      vclock = VClockN,
                      polog = POLogNew}.

-spec event_remote(NodeIdRemote :: node_id(),
                   VClockRemote :: vclock(),
                   Operation :: operation_write(),
                   State :: state(),
                   Dispatcher :: cloudi_service:dispatcher()) ->
    {{vclock, node_id(), vclock()}, state()}.

event_remote(NodeIdRemote, VClockRemote, Operation,
             #cloudi_crdt{service_name_full = ServiceNameFull,
                          node_id = NodeId,
                          polog_mode = bootstrap} = State,
             Dispatcher) ->
    StateNew = case bootstrap_local_done(NodeIdRemote, VClockRemote, Operation,
                                         State, Dispatcher) of
        {true, StateNext} ->
            StateNext;
        {false, #cloudi_crdt{queue = Queue,
                             vclock = VClock} = StateNext} ->
            {ok, QueueNew} = cloudi_queue:
                             mcast(Dispatcher, ServiceNameFull,
                                   {vclock, NodeId, VClock}, Queue),
            VClockNext = vclock_increment(NodeId, VClock), % send
            StateNext#cloudi_crdt{queue = QueueNew,
                                  vclock = VClockNext}
    end,
    #cloudi_crdt{vclock = VClockNew} = StateNew,
    {{vclock, NodeId, VClockNew}, StateNew};
event_remote(NodeIdRemote, VClockRemote0, Operation,
             #cloudi_crdt{node_id = NodeId,
                          node_ids = NodeIds,
                          vclock = VClock0,
                          vclocks = VClocks,
                          polog_mode = normal,
                          polog = POLog,
                          data = Data} = State, _) ->
    % A remote write operation is received and added to the POLog
    % (from a CloudI service request sent by the broadcast in event_local/3).
    % The current vclock() is provided as a response for this
    % CloudI service request in the broadcast.

    VClockRemoteN = vclock_current(NodeIds, VClockRemote0),
    VClock1 = vclock_merge(VClockRemoteN,
                           vclock_increment(NodeId, VClock0)), % receive
    POLogNext = polog_effect(Operation, VClock1, POLog),
    VClockN = vclock_increment(NodeId, VClock1), % event
    VClocksNew = vclocks_update(NodeId, VClockN,
                                vclocks_update(NodeIdRemote, VClockRemoteN,
                                               VClocks)),
    VClockMin = vclocks_minimum(VClocksNew),
    {POLogNew, DataNew} = polog_stable(POLogNext, Data, VClockMin),
    {{vclock, NodeId, VClockN},
     State#cloudi_crdt{vclock = VClockN,
                       vclocks = VClocksNew,
                       polog = POLogNew,
                       data = DataNew}}.

-spec event_local_vclock(NodeIdRemote :: node_id(),
                         VClockRemote :: vclock(),
                         State :: state(),
                         Dispatcher :: cloudi_service:dispatcher()) ->
    state().

event_local_vclock(NodeIdRemote, VClockRemote,
                   #cloudi_crdt{service_name_full = ServiceNameFull,
                                polog_mode = bootstrap} = State,
                   Dispatcher) ->
    StateNew = bootstrap_local(NodeIdRemote, VClockRemote, State),
    #cloudi_crdt{queue = Queue,
                 node_id = NodeId,
                 vclock = VClock0} = StateNew,
    {ok, QueueNew} = cloudi_queue:
                     mcast(Dispatcher, ServiceNameFull,
                           {vclock, NodeId, VClock0}, Queue),
    VClockN = vclock_increment(NodeId, VClock0), % send
    StateNew#cloudi_crdt{queue = QueueNew,
                         vclock = VClockN};
event_local_vclock(NodeIdRemote, VClockRemote0,
                   #cloudi_crdt{service_name_full = ServiceNameFull,
                                queue = Queue,
                                node_id = NodeId,
                                node_ids = NodeIds,
                                vclock = VClock0,
                                vclocks = VClocks,
                                polog_mode = normal,
                                polog = POLog,
                                data = Data} = State,
                   Dispatcher) ->
    % Update the vclock() from the local operation broadcast response
    % that was provided by event_remote/4.

    VClockRemoteN = vclock_current(NodeIds, VClockRemote0),
    VClock1 = vclock_merge(VClockRemoteN,
                           vclock_increment(NodeId, VClock0)), % receive
    {QueueNew, VClockN} = case cloudi_queue:size(Queue) of
        0 ->
            % If nothing else is currently being sent,
            % send the current vclock to all the processes.
            % This is executed after an operation has completed its
            % broadcast and no other operations are ready to broadcast.
            {ok, QueueNext} = cloudi_queue:
                              mcast(Dispatcher, ServiceNameFull,
                                    {vclock, NodeId, VClock1}, Queue),
            {QueueNext, vclock_increment(NodeId, VClock1)}; % send
        _ ->
            {Queue, VClock1}
    end,
    VClocksNew = vclocks_update(NodeId, VClockN,
                                vclocks_update(NodeIdRemote, VClockRemoteN,
                                               VClocks)),
    VClockMin = vclocks_minimum(VClocksNew),
    {POLogNew, DataNew} = polog_stable(POLog, Data, VClockMin),
    State#cloudi_crdt{queue = QueueNew,
                      vclock = VClockN,
                      vclocks = VClocksNew,
                      polog = POLogNew,
                      data = DataNew}.

-spec event_remote_vclock(NodeIdRemote :: node_id(),
                          VClockRemote :: vclock(),
                          State :: state(),
                          Dispatcher :: cloudi_service:dispatcher()) ->
    {{vclock_updated, node_id(), vclock()}, state()}.

event_remote_vclock(NodeIdRemote, VClockRemote,
                    #cloudi_crdt{polog_mode = bootstrap} = State, _) ->
    StateNew = bootstrap_local(NodeIdRemote, VClockRemote, State),
    #cloudi_crdt{node_id = NodeId,
                 vclock = VClock} = StateNew,
    {{vclock_updated, NodeId, VClock}, StateNew};
event_remote_vclock(NodeIdRemote, VClockRemote0,
                    #cloudi_crdt{service_name_full = ServiceNameFull,
                                 queue = Queue,
                                 node_id = NodeId,
                                 node_ids = NodeIds,
                                 vclock = VClock0,
                                 vclocks = VClocks,
                                 polog_mode = normal,
                                 polog = POLog,
                                 data = Data} = State,
                    Dispatcher) ->
    % The vclock() broadcasted from event_local_vclock/4 updates the
    % the remote CloudI service process here.  The updated vclock() is
    % broadcasted if no other operations are being sent, to ensure the
    % the operation takes effect on all CloudI service processes as
    % quick as possible.

    VClockRemoteN = vclock_current(NodeIds, VClockRemote0),
    VClock1 = vclock_merge(VClockRemoteN,
                           vclock_increment(NodeId, VClock0)), % receive
    {QueueNew, VClockN} = case cloudi_queue:size(Queue) of
        0 ->
            % If nothing else is currently being sent,
            % send the current vclock to all the processes.
            {ok, QueueNext} = cloudi_queue:
                              mcast(Dispatcher, ServiceNameFull,
                                    {vclock_updated, NodeId, VClock1}, Queue),
            {QueueNext, vclock_increment(NodeId, VClock1)}; % send
        _ ->
            {Queue, VClock1}
    end,
    VClocksNew = vclocks_update(NodeId, VClockN,
                                vclocks_update(NodeIdRemote, VClockRemoteN,
                                               VClocks)),
    VClockMin = vclocks_minimum(VClocksNew),
    {POLogNew, DataNew} = polog_stable(POLog, Data, VClockMin),
    {{vclock_updated, NodeId, VClockN},
     State#cloudi_crdt{queue = QueueNew,
                       vclock = VClockN,
                       vclocks = VClocksNew,
                       polog = POLogNew,
                       data = DataNew}}.

-spec event_local_vclock_updated(NodeIdRemote :: node_id(),
                                 VClockRemote :: vclock(),
                                 State :: state()) ->
    state().

event_local_vclock_updated(_, _,
                           #cloudi_crdt{polog_mode = bootstrap} = State) ->
    State;
event_local_vclock_updated(NodeIdRemote, VClockRemote0,
                           #cloudi_crdt{node_id = NodeId,
                                        node_ids = NodeIds,
                                        vclock = VClock0,
                                        vclocks = VClocks,
                                        polog_mode = normal,
                                        polog = POLog,
                                        data = Data} = State) ->
    % Update the vclock() from the local vclock() broadcast response
    % that was provided by event_remote_vclock/4.

    VClockRemoteN = vclock_current(NodeIds, VClockRemote0),
    VClockN = vclock_merge(VClockRemoteN,
                           vclock_increment(NodeId, VClock0)), % receive
    VClocksNew = vclocks_update(NodeId, VClockN,
                                vclocks_update(NodeIdRemote, VClockRemoteN,
                                               VClocks)),
    VClockMin = vclocks_minimum(VClocksNew),
    {POLogNew, DataNew} = polog_stable(POLog, Data, VClockMin),
    State#cloudi_crdt{vclock = VClockN,
                      vclocks = VClocksNew,
                      polog = POLogNew,
                      data = DataNew}.

-spec event_remote_vclock_updated(NodeIdRemote :: node_id(),
                                  VClockRemote :: vclock(),
                                  State :: state()) ->
    {vclock_updated, state()}.

event_remote_vclock_updated(NodeIdRemote, VClockRemote,
                            #cloudi_crdt{polog_mode = bootstrap} = State) ->
    {vclock_updated,
     bootstrap_local(NodeIdRemote, VClockRemote, State)};
event_remote_vclock_updated(NodeIdRemote, VClockRemote,
                            #cloudi_crdt{polog_mode = normal} = State) ->
    % The vclock() broadcasted from event_remote_vclock/4 updates the
    % the remote CloudI service process here.

    {vclock_updated,
     event_local_vclock_updated(NodeIdRemote, VClockRemote, State)}.

-spec bootstrap_local(NodeIdRemote :: node_id(),
                      VClockRemote :: vclock(),
                      State :: state()) ->
    state().

bootstrap_local(NodeIdRemote, VClockRemote,
                #cloudi_crdt{node_id = NodeId,
                             node_ids = NodeIds,
                             vclock = VClock0,
                             polog_mode = bootstrap} = State) ->
    VClockN = vclock_merge(VClockRemote,
                           vclock_increment(NodeId, VClock0)), % receive
    State#cloudi_crdt{node_ids = lists:umerge(NodeIds, [NodeIdRemote]),
                      vclock = VClockN}.

-spec bootstrap_local_done(NodeIdRemote :: node_id(),
                           VClockRemote :: vclock(),
                           Operation :: operation_write(),
                           State :: state(),
                           Dispatcher :: cloudi_service:dispatcher()) ->
    {boolean(), state()}.

bootstrap_local_done(NodeIdRemote, VClockRemote, Operation,
                     #cloudi_crdt{service_name_full = ServiceNameFull,
                                  node_id = NodeId,
                                  polog_mode = bootstrap} = State,
                     Dispatcher) ->
    StateNext = bootstrap_local(NodeIdRemote, VClockRemote, State),
    #cloudi_crdt{node_ids = NodeIdsOld,
                 vclock = VClock0,
                 polog = POLog} = StateNext,
    POLogNew = polog_effect(Operation, VClock0, POLog),
    VClockN = vclock_increment(NodeId, VClock0), % event
    StateNew = StateNext#cloudi_crdt{vclock = VClockN,
                                     polog = POLogNew},
    case cloudi_service:get_pids(Dispatcher, ServiceNameFull, undefined) of
        {ok, PatternPids} ->
            NodeIdsNew = lists:foldl(fun({_, Pid}, NodeIdsNext) ->
                lists:umerge(NodeIdsNext, [node_id(Pid)])
            end, [NodeId], PatternPids),
            BootstrapUpdate = (NodeIdsNew -- NodeIdsOld == []) andalso
                lists:member(NodeIdRemote, NodeIdsNew),
            if
                BootstrapUpdate =:= true ->
                    bootstrap_update(PatternPids, NodeIdRemote,
                                     StateNew#cloudi_crdt{
                                         node_ids = NodeIdsNew},
                                     Dispatcher);
                BootstrapUpdate =:= false ->
                    {false, StateNew}
            end;
        {error, timeout} ->
            ?LOG_WARN("bootstrap_done timeout", []),
            {false, StateNew}
    end.

-spec bootstrap_update(PatternPids :: list(cloudi_service:pattern_pid()),
                       NodeIdRemoteBlocked :: node_id(),
                       State :: state(),
                       Dispatcher :: cloudi_service:dispatcher()) ->
    {boolean(), state()}.

bootstrap_update(PatternPids, NodeIdRemoteBlocked,
                 #cloudi_crdt{service_name_full = ServiceNameFull,
                              clean_vclocks_interval = CleanInterval,
                              node_id = NodeId,
                              node_ids = NodeIds,
                              vclock = VClock0,
                              polog_mode = bootstrap} = State,
                 Dispatcher) ->
    case bootstrap_update_get(PatternPids, NodeIdRemoteBlocked, NodeIds,
                              VClock0, ServiceNameFull, Dispatcher) of
        bootstrap_done ->
            % All the CloudI service processes have been started recently
            % based on all the vclock() integers (so all are effectively
            % in the bootstrap POLogMode at the same time and will find
            % agreement on the NodeIds here).
            StateNew = clean_vclocks_store(State),
            ok = clean_vclocks_send(CleanInterval * 1000, NodeId),
            {true, StateNew#cloudi_crdt{polog_mode = normal}};
        {ok, VClockRemote, VClocks, POLog, Data} ->
            % Use CRDT state from the NodeIdRemoteBlocked process.
            VClockN = vclock_merge(VClockRemote,
                                   vclock_increment(NodeId, VClock0)), % event
            StateNew = clean_vclocks_store(State#cloudi_crdt{vclock = VClockN,
                                                             vclocks = VClocks,
                                                             polog = POLog,
                                                             data = Data}),
            ok = clean_vclocks_send(CleanInterval * 1000, NodeId),
            {true, StateNew#cloudi_crdt{polog_mode = normal}};
        {error, _} ->
            {false, State}
    end.

-spec bootstrap_update_get(PatternPids :: list(cloudi_service:pattern_pid()),
                           NodeIdRemoteBlocked :: node_id(),
                           NodeIds :: list(node_id()),
                           VClock :: vclock(),
                           ServiceNameFull :: cloudi_service:service_name(),
                           Dispatcher :: cloudi_service:dispatcher()) ->
    bootstrap_done |
    {ok, vclock(), vclocks(), polog(), data()} |
    {error, update_invalid | timeout}.

bootstrap_update_get(PatternPids, NodeIdRemoteBlocked, NodeIds,
                     VClock, ServiceNameFull, Dispatcher) ->
    bootstrap_update_get(PatternPids, [], 0, NodeIdRemoteBlocked, NodeIds,
                         VClock, ServiceNameFull, Dispatcher).

bootstrap_update_get([], TransIds, Count, NodeIdRemoteBlocked, NodeIds,
                     VClock, _, Dispatcher) ->
    case cloudi_service:recv_asyncs(Dispatcher, TransIds) of
        {ok, Recvs} ->
            UpdateStates = [{vclock_average(vclock_current(NodeIds,
                                                           VClockRemote)),
                             NodeIdRemote, VClockRemote, VClocksRemote,
                             POLogModeRemote, POLogRemote, DataRemote}
                            || {_,
                                {state,
                                 NodeIdRemote, VClockRemote, VClocksRemote,
                                 POLogModeRemote, POLogRemote, DataRemote},
                                _} <- Recvs],
            VClockAvg = vclock_average(vclock_current(NodeIds, VClock)),
            bootstrap_update_select(UpdateStates, Count,
                                    NodeIdRemoteBlocked, VClockAvg);
        {error, timeout} = Error ->
            ?LOG_WARN("bootstrap_update recv_asyncs timeout", []),
            Error
    end;
bootstrap_update_get([PatternPid | PatternPids], TransIds, Count,
                     NodeIdRemoteBlocked, NodeIds,
                     VClock, ServiceNameFull, Dispatcher) ->
    case cloudi_service:send_async(Dispatcher, ServiceNameFull, <<>>,
                                   state, undefined, undefined, PatternPid) of
        {ok, TransId} ->
            bootstrap_update_get(PatternPids,
                                 [TransId | TransIds], Count + 1,
                                 NodeIdRemoteBlocked, NodeIds,
                                 VClock, ServiceNameFull, Dispatcher);
        {error, timeout} = Error ->
            ?LOG_WARN("bootstrap_update send_async timeout", []),
            Error
    end.

-spec bootstrap_update_select(UpdateStates :: list({float(),
                                                    node_id(),
                                                    vclock(), vclocks(),
                                                    polog_mode(), polog(),
                                                    data()}),
                              Count :: non_neg_integer(),
                              NodeIdRemoteBlocked :: node_id(),
                              VClockAvg :: float()) ->
    bootstrap_done |
    {ok, vclock(), vclocks(), polog(), data()} |
    {error, update_invalid}.

bootstrap_update_select(UpdateStates, Count,
                        NodeIdRemoteBlocked, VClockAvg) ->
    VClockAvgL = [VClockAvg |
                  [VClockAvgRemote
                   || {VClockAvgRemote, _, _, _, _, _, _} <- UpdateStates]],
    AllNewProcesses = lists:all(fun(VClockAvgValue) ->
        % The comparison here uses an arbitrary number based on testing
        % to determine what a new process really is.
        VClockAvgValue < 80
    end, VClockAvgL),
    if
        AllNewProcesses =:= true ->
            bootstrap_done;
        AllNewProcesses =:= false ->
            % The CRDT bootstrap POLogMode is not due to an initial startup
            % of a CloudI service, but is instead due to a restart or the
            % start of a new CloudI service instance.
            % (assumes a normal distribution of vclock() averages exists)
            VClockAllAvg = lists:sum(VClockAvgL) / (Count + 1),
            VClockAllAvgStdDev = math:pow(lists:foldl(fun(VClockAvgValue,
                                                          SqsSum) ->
                Diff = VClockAvgValue - VClockAllAvg,
                Diff * Diff + SqsSum
            end, 0, VClockAvgL) / Count, 0.5), % sample stddev

            % 50% is 0.67448975019608171 * stddev
            VClockAllAvgThreshold = VClockAllAvg -
                VClockAllAvgStdDev * 0.67448975019608171,
            case lists:keyfind(NodeIdRemoteBlocked, 2, UpdateStates) of
                {VClockAvgRemote, NodeIdRemoteBlocked,
                 VClockN, VClocksN, normal, POLogN, DataN}
                    when VClockAvgRemote >= VClockAllAvgThreshold ->
                    {ok, VClockN, VClocksN, POLogN, DataN};
                {_, _, _, _, _, _, _} ->
                    % The blocked node_id() was too young when compared to
                    % the other node_ids.
                    {error, update_invalid}
            end
    end.

-spec polog_effect(Operation :: operation_write(),
                   VClock :: vclock(),
                   POLog :: polog()) ->
    polog().

polog_effect(Operation, VClock, POLog0) ->
    case polog_duplicate_operation(VClock, POLog0) of
        true ->
            POLog0;
        false ->
            case polog_redundancy_relation(Operation, VClock, POLog0) of
                {ignore, POLogN} ->
                    POLogN;
                {add, POLogN} ->
                    [{VClock, Operation} | POLogN]
            end
    end.

-spec polog_stable(POLog :: polog(),
                   Data :: data(),
                   VClockMin :: vclock()) ->
    {polog(), data()}.

polog_stable(POLog, Data, VClockMin) ->
    polog_stable(lists:reverse(POLog), [], Data, VClockMin).

polog_stable([], POLogNew, Data, _) ->
    {POLogNew, Data};
polog_stable([{VClock, Operation} = POLogValue | POLogOld], POLogNew,
             Data, VClockMin) ->
    case vclock_less_than(VClock, VClockMin) of
        true ->
            polog_stable(POLogOld, POLogNew,
                         write(Operation, Data), VClockMin);
        false ->
            polog_stable(POLogOld, [POLogValue | POLogNew],
                         Data, VClockMin)
    end.

-spec polog_duplicate_operation(VClock :: vclock(),
                                POLog :: polog()) ->
    boolean().

polog_duplicate_operation(VClock, POLog) ->
    % If cloudi_queue was set to do retries of CloudI service requests,
    % it is possible that a failure to send an operation causes an operation
    % to be sent more than once, if it gets received the first time with
    % a response failure. The vclock() value is unique, and vclock() updates
    % are required to remove the operation from the POLog
    % (i.e., the operation must still be in the POLog),
    % so it is easy to check for.
    case lists:keyfind(VClock, 1, POLog) of
        {VClock, _} ->
            true;
        false ->
            false
    end.

-spec polog_redundancy_relation(Operation :: operation_write(),
                                VClock :: vclock(),
                                POLog :: polog()) ->
    {add | ignore, POLogNew :: polog()}.

polog_redundancy_relation({assign, _, _}, _, POLog) ->
    % assign can not be determined to be redundant because its
    % effect is only determined once state is consistent
    % (any number of put or clear operations may compete to determine
    %  whether an assign operation may succeed at a later point in time).
    {add, POLog};
polog_redundancy_relation({incr, _, _}, _, POLog) ->
    % Both incr and decr only mutate existing data and are unable to be
    % redundant, unless there is an incr/decr pair that contain the same
    % Key and Value (or some combination is equivalent to this).
    % The occurrence of redundant incr/decr will be infrequent and it is
    % best to avoid the extra processing a check would require.
    {add, POLog};
polog_redundancy_relation({decr, _, _}, _, POLog) ->
    {add, POLog};
polog_redundancy_relation({update, _, _, _, _}, _, POLog) ->
    % Update may contain any operation that operates on a value,
    % if a value exists, so it is unable to be redundant
    % (similar to incr and decr, but more generic).
    {add, POLog};
polog_redundancy_relation({update, _, _, _, _, _}, _, POLog) ->
    {add, POLog};
polog_redundancy_relation({put, Key, _}, VClock, POLog) ->
    % only removes the first redundant operation to prevent memory growth
    polog_redundancy_relation_put(POLog, [], Key, VClock);
polog_redundancy_relation({clear, Key}, VClock, POLog) ->
    % only removes the first redundant operation to prevent memory growth
    polog_redundancy_relation_clear(POLog, [], Key, VClock);
polog_redundancy_relation(clear_all, VClock, POLog) ->
    polog_redundancy_relation_clear_all(POLog, [], VClock).

polog_redundancy_relation_put([], POLog, _, _) ->
    {add, lists:reverse(POLog)};
polog_redundancy_relation_put([{VClock,
                                {put, Key, _}} | POLogWithout] = POLogWith,
                              POLog, Key, VClockNow) ->
    polog_redundancy_relation_conflict_resolve(POLogWith, POLogWithout, POLog,
                                               VClock, VClockNow);
polog_redundancy_relation_put([{VClock,
                                {clear, Key}} | POLogWithout] = POLogWith,
                              POLog, Key, VClockNow) ->
    polog_redundancy_relation_conflict_resolve(POLogWith, POLogWithout, POLog,
                                               VClock, VClockNow);
polog_redundancy_relation_put([POLogValue | POLogWithout],
                              POLog, Key, VClockNow) ->
    polog_redundancy_relation_put(POLogWithout,
                                  [POLogValue | POLog], Key, VClockNow).

polog_redundancy_relation_clear([], POLog, _, _) ->
    {add, lists:reverse(POLog)};
polog_redundancy_relation_clear([{VClock,
                                  {put, Key, _}} | POLogWithout] = POLogWith,
                                POLog, Key, VClockNow) ->
    polog_redundancy_relation_conflict_resolve(POLogWith, POLogWithout, POLog,
                                               VClock, VClockNow);
polog_redundancy_relation_clear([{VClock,
                                  {clear, Key}} | POLogWithout] = POLogWith,
                                POLog, Key, VClockNow) ->
    polog_redundancy_relation_conflict_resolve(POLogWith, POLogWithout, POLog,
                                               VClock, VClockNow);
polog_redundancy_relation_clear([POLogValue | POLogWithout],
                                POLog, Key, VClockNow) ->
    polog_redundancy_relation_clear(POLogWithout,
                                    [POLogValue | POLog], Key, VClockNow).

polog_redundancy_relation_clear_all([], POLog, _) ->
    {add, lists:reverse(POLog)};
polog_redundancy_relation_clear_all([{VClock, _} = POLogValue | POLogWithout],
                                    POLog, VClockNow) ->
    case vclock_less_than(VClock, VClockNow) of
        true ->
            polog_redundancy_relation_clear_all(POLogWithout, POLog,
                                                VClockNow);
        false ->
            polog_redundancy_relation_clear_all(POLogWithout,
                                                [POLogValue | POLog],
                                                VClockNow)
    end.

polog_redundancy_relation_conflict_resolve(POLogWith, POLogWithout, POLog,
                                           VClock, VClockNow) ->
    case vclock_less_than(VClock, VClockNow) of
        true ->
            {add, lists:reverse(POLog, POLogWithout)};
        false ->
            {ignore, lists:reverse(POLog, POLogWith)}
    end.

-spec read(operation_read(),
           Data :: data()) ->
    any().

read({find, Key}, Data) ->
    maps:find(Key, Data);
read({fold, F, AccInit}, Data) ->
    maps:fold(F, AccInit, Data);
read({get, Key}, Data) ->
    maps:get(Key, Data);
read({is_key, Key}, Data) ->
    maps:is_key(Key, Data);
read(keys, Data) ->
    maps:keys(Data);
read(size, Data) ->
    maps:size(Data);
read(values, Data) ->
    maps:values(Data).

-spec write(Operation :: operation_write(),
            Data :: data()) ->
    DataNew :: data().

write({assign, Key, Value}, Data) ->
    maps:update_with(Key, fun(ValueOld) ->
        ValueOld
    end, Value, Data);
write({incr, Key, Value}, Data) ->
    try maps:update_with(Key, fun(ValueOld) ->
            if
                is_number(ValueOld) ->
                    ValueOld + Value;
                true ->
                    ValueOld
            end
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({decr, Key, Value}, Data) ->
    try maps:update_with(Key, fun(ValueOld) ->
            if
                is_number(ValueOld) ->
                    ValueOld - Value;
                true ->
                    ValueOld
            end
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({update, Key, ModuleVersion, Module, Function}, Data) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 1),
    try maps:update_with(Key, fun(ValueOld) ->
            Module:Function(ValueOld)
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({update, Key, ModuleVersion, Module, Function, Argument1}, Data) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 2),
    try maps:update_with(Key, fun(ValueOld) ->
            Module:Function(Argument1, ValueOld)
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({put, Key, Value}, Data) ->
    maps:put(Key, Value, Data);
write({clear, Key}, Data) ->
    maps:remove(Key, Data);
write(clear_all, _) ->
    maps:new().

-spec clean_vclocks_send(Interval :: milliseconds(),
                         NodeId :: node_id()) ->
    ok.

clean_vclocks_send(Interval, {_, Service}) ->
    _ = erlang:send_after(Interval, Service, cloudi_crdt_clean_vclocks),
    ok.

-spec clean_vclocks(State :: state(),
                    Dispatcher :: cloudi_service:dispatcher()) ->
    state().

clean_vclocks(#cloudi_crdt{service_name_full = ServiceNameFull,
                           clean_vclocks_interval = CleanInterval,
                           clean_vclocks_failure = CleanFailure,
                           node_id = NodeId,
                           node_ids = NodeIdsOld} = State,
              Dispatcher) ->
    case cloudi_service:get_pids(Dispatcher, ServiceNameFull, undefined) of
        {ok, PatternPids} ->
            NodeIdsOldCount = length(NodeIdsOld),
            NodeIdsNewCount = length(PatternPids) + 1,
            StateNew = if
                NodeIdsOldCount > NodeIdsNewCount,
                (NodeIdsOldCount - NodeIdsNewCount) * 100 /
                NodeIdsOldCount >= CleanFailure ->
                    ?LOG_WARN("clean_vclocks_failure ~w met (~w -> ~w), "
                              "clean_vclocks is delayed",
                              [CleanFailure,
                               NodeIdsOldCount, NodeIdsNewCount]),
                    State;
                true ->
                    NodeIdsNew = lists:foldl(fun({_, Pid}, NodeIdsNext) ->
                        lists:umerge(NodeIdsNext, [node_id(Pid)])
                    end, [NodeId], PatternPids),
                    clean_vclocks_store(State#cloudi_crdt{
                                            node_ids = NodeIdsNew})
            end,
            ok = clean_vclocks_send(CleanInterval * 1000, NodeId),
            StateNew;
        {error, timeout} ->
            ?LOG_WARN("clean_vclocks timeout", []),
            State
    end.

-spec clean_vclocks_store(State :: state()) ->
    state().

clean_vclocks_store(#cloudi_crdt{node_id = NodeId,
                                 node_ids = NodeIds,
                                 vclock = VClock0,
                                 vclocks = VClocks0,
                                 polog = POLog0,
                                 data = Data0} = State) ->
    VClockN = vclock_current(NodeIds, VClock0),
    VClocks1 = vclocks_update(NodeId, VClockN, VClocks0),
    VClocksN = vclocks_current(NodeIds, VClocks1),
    POLog1 = [{vclock_current(NodeIds, VClockPOLog), Operation} ||
              {VClockPOLog, Operation} <- POLog0],
    VClockMin = vclocks_minimum(VClocksN),
    {POLogN, DataN} = polog_stable(POLog1, Data0, VClockMin),
    State#cloudi_crdt{vclock = VClockN,
                      vclocks = VClocksN,
                      polog = POLogN,
                      data = DataN}.

-spec vclock_current(NodeIds :: list(node_id()),
                     VClock :: vclock()) ->
    vclock().

vclock_current(NodeIds, VClock) ->
    maps:with(NodeIds, VClock).

-spec vclock_increment(NodeId :: node_id(),
                       VClock :: vclock()) ->
    vclock().

vclock_increment(NodeId, VClock) ->
    maps:update_with(NodeId, fun(Clock) ->
        Clock + 1
    end, 0, VClock).

-spec vclock_less_than(VClockA :: vclock(),
                       VClockB :: vclock()) ->
    boolean().

vclock_less_than(VClockA, VClockB) ->
    try maps:fold(fun(NodeIdA, ClockA, True) ->
            case maps:find(NodeIdA, VClockB) of
                {ok, ClockB}
                    when ClockA < ClockB ->
                    True;
                _ ->
                    erlang:throw(false)
            end
        end, true, VClockA)
    catch
        throw:false ->
            false
    end.

-spec vclock_merge(VClockA :: vclock(),
                   VClockB :: vclock()) ->
    vclock().

vclock_merge(VClockA, VClockB) ->
    maps_merge(fun(_, ClockA, ClockB) ->
        erlang:max(ClockA, ClockB)
    end, VClockA, VClockB).

-spec vclock_minimum(VClockA :: vclock(),
                     VClockB :: vclock()) ->
    vclock().

vclock_minimum(VClockA, VClockB) ->
    maps_merge(fun(_, ClockA, ClockB) ->
        erlang:min(ClockA, ClockB)
    end, VClockA, VClockB).

-spec vclock_new() ->
    vclock().

vclock_new() ->
    #{}.

-spec vclock_average(VClock :: vclock()) ->
    float().

vclock_average(VClock) ->
    maps:fold(fun(_, Value, Sum) ->
        Sum + Value
    end, 0, VClock) / maps:size(VClock).

-spec vclocks_current(NodeIds :: list(node_id()),
                      VClocks :: vclocks()) ->
    vclocks().

vclocks_current(NodeIds, VClocks) ->
    maps:map(fun(_, VClock) ->
        vclock_current(NodeIds, VClock)
    end, maps:with(NodeIds, VClocks)).

-spec vclocks_new() ->
    vclocks().

vclocks_new() ->
    #{}.

-spec vclocks_minimum(VClocks :: vclocks()) ->
    vclock().

vclocks_minimum(VClocks) ->
    maps:fold(fun(_, VClock, VClockMin) ->
        vclock_minimum(VClock, VClockMin)
    end, vclock_new(), VClocks).

-spec vclocks_update(NodeId :: node_id(),
                     VClock :: vclock(),
                     VClocks :: vclocks()) ->
    vclocks().

vclocks_update(NodeId, VClock, VClocks) ->
    maps:put(NodeId, VClock, VClocks).

maps_merge(F, M1, M2) ->
    maps:fold(fun(K, V1, M) ->
                  maps:update_with(K, fun(V2) -> F(K, V1, V2) end, V1, M)
              end, M2, M1).

