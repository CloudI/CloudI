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
%%% The cloudi_crdt functions that may be called within
%%% cloudi_service_init/4 are events_subscribe/3, events_subscribe/4,
%%% events_subscriptions/3, events_clear/2, events_clear/3, new/1 and new/2.
%%% A CloudI service that uses cloudi_crdt should have a
%%% destination refresh method that is immediate.
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
%%% Copyright (c) 2017-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2017-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_crdt).
-author('mjtruog at protonmail dot com').

%% external interface
-export([assign/4,
         assign_id/5,
         byte_size/2,
         clear/2,
         clear/3,
         clear_id/3,
         clear_id/4,
         decr/3,
         decr/4,
         decr_id/4,
         decr_id/5,
         events_subscribe/3,
         events_subscribe/4,
         events_subscriptions/3,
         events_clear/2,
         events_clear/3,
         find/3,
         fold/4,
         get/3,
         handle_info/3,
         handle_request/11,
         incr/3,
         incr/4,
         incr_id/4,
         incr_id/5,
         is_key/3,
         keys/2,
         new/1,
         new/2,
         put/4,
         put_id/5,
         size/2,
         update/5,
         update/6,
         update_id/6,
         update_id/7,
         update_assign/6,
         update_assign/7,
         update_assign_id/7,
         update_assign_id/8,
         update_clear/5,
         update_clear/6,
         update_clear_id/6,
         update_clear_id/7,
         values/2,
         zero/3,
         zero_id/4]).

-include("cloudi_crdt.hrl").
-include("cloudi_service.hrl").
-include("cloudi_core_i_constants.hrl").
-include("cloudi_logger.hrl").

-type node_id() :: {node(), cloudi_service:source()}.
-type vclock() :: #{node_id() := non_neg_integer()}.
-type vclocks() :: #{node_id() := vclock()}.
-type seconds() :: 1 .. 4294967.
-type milliseconds() :: 1 .. 4294967295.
-type key() :: any().
-type value() :: any().
-type operation_read() ::
    {find,          Key :: key()} |
    {fold,          F :: fun((Key :: key(), Value :: value(),
                              AccIn :: any()) -> AccOut :: any()),
                    AccInit :: any()} |
    {get,           Key :: key()} |
    {is_key,        Key :: key()} |
    keys |
    size |
    values.
-type operation_write() ::
    {assign,        Id :: event_id(), Key :: key(), Value :: value()} |
    {incr,          Id :: event_id(), Key :: key(), Value :: value()} |
    {decr,          Id :: event_id(), Key :: key(), Value :: value()} |
    {update,        Id :: event_id(), Key :: key(),
                    ModuleVersion :: list(),
                    Module :: module(), Function :: atom()} |
    {update,        Id :: event_id(), Key :: key(),
                    ModuleVersion :: list(),
                    Module :: module(), Function :: atom(),
                    Argument1 :: any()} |
    {update_assign, Id :: event_id(), Key :: key(), Value :: value(),
                    ModuleVersion :: list(),
                    Module :: module(), Function :: atom()} |
    {update_assign, Id :: event_id(), Key :: key(), Value :: value(),
                    ModuleVersion :: list(),
                    Module :: module(), Function :: atom(),
                    Argument1 :: any()} |
    {update_clear,  Id :: event_id(), Key :: key(),
                    ModuleVersion :: list(),
                    Module :: module(), Function :: atom()} |
    {update_clear,  Id :: event_id(), Key :: key(),
                    ModuleVersion :: list(),
                    Module :: module(), Function :: atom(),
                    Argument1 :: any()} |
    {put,           Id :: event_id(), Key :: key(), Value :: value()} |
    {clear,         Id :: event_id(), Key :: key()} |
    {clear_all,     Id :: event_id()}.

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

% cloudi_crdt state from other CloudI service processes that is used to
% determine when bootstrap mode is done
-type bootstrap_state() ::
    {VClockAvg :: float(),
     NodeId :: node_id(),
     VClock :: vclock(),
     VClocks :: vclocks(),
     POLogMode :: polog_mode(),
     POLog :: polog(),
     Data :: data()}.

% The POLog data type that contains consistent state (an Erlang map)
-type data() :: #{key() := value()}.

% Events are changes to data()
-type events() :: #{key() := list(event_type())}.

% Initial data function to process data received after a restart
% (data that won't be provided in events
%  because the data is already stored remotely)
-type initial_data_function() :: fun((data()) -> any()).

-record(cloudi_crdt,
    {
        service_name_full :: cloudi_service:service_name(),
        initial_data_function :: undefined | initial_data_function(),
        clean_vclocks_interval :: seconds(),
        clean_vclocks_failure :: number(),
        queue :: cloudi_queue:state(),
        word_size :: pos_integer(),
        node_id :: node_id(),
        node_ids :: list(node_id()),
        vclock :: vclock(),
        vclocks = vclocks_new() :: vclocks(),
        polog_mode = bootstrap :: polog_mode(),
        bootstrap_node_id = undefined :: undefined | node_id(),
        bootstrap_states = [] :: list(bootstrap_state()),
        bootstrap_requests = 0 :: non_neg_integer(),
        polog = [] :: polog(),
        data = #{} :: data(),
        events = #{} :: events(),
        events_any = [] :: list(event_type())
    }).

-define(DEFAULT_SERVICE_NAME,                    "crdt").
-define(DEFAULT_INITIAL_DATA_FUNCTION,        undefined).
        % Provide an arity-1 function to read internal Erlang map data
        % received after a restart before events are processed
        % (for data that will not be present in events).
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
-define(DEFAULT_TIMEOUT_DEFAULT,              undefined).
        % provide a default timeout that will be used instead of
        % the timeout_async service configuration value when the
        % timeout is provided as undefined
-define(DEFAULT_PRIORITY_DEFAULT,             undefined).
        % provide a default priority that will be used instead of
        % the service configuration option priority_default when the
        % priority is provided as undefined
-define(DEFAULT_PRIORITY_DEFAULT_OFFSET,      undefined).
        % provide an offset that is added to the service configuration
        % option priority_default to set a default priority that will be
        % used when priority is provided as undefined

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type event_type() ::
    assign |
    clear |
    decr |
    incr |
    put |
    update.
-define(EVENT_TYPES, [assign, clear, decr, incr, put, update]).
-type event_id() :: cloudi_service:trans_id() | any().
-type options() ::
    list({service_name, string()} |
         {initial_data_function, initial_data_function() | undefined} |
         {clean_vclocks, seconds()} |
         {clean_vclocks_failure, float() | 1..100} |
         {retry, non_neg_integer()} |
         {retry_delay, non_neg_integer()} |
         {timeout_default, cloudi_service:timeout_milliseconds()} |
         {priority_default, cloudi_service:priority()} |
         {priority_default_offset,
          ?PRIORITY_HIGHER_OFFSET..?PRIORITY_LOWER_OFFSET | undefined}).
-type state() :: #cloudi_crdt{}.
-export_type([event_type/0,
              event_id/0,
              options/0,
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

assign(Dispatcher, Key, Value, State) ->
    assign_id(Dispatcher, Key, Value, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Assign a value iff none exists in the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec assign_id(Dispatcher :: cloudi_service:dispatcher(),
                Key :: key(),
                Value :: value(),
                Id :: event_id(),
                State :: state()) ->
    state().

assign_id(Dispatcher, Key, Value, Id, State)
    when is_pid(Dispatcher) ->
    event_local({assign, Id, Key, Value}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the size of the CloudI CRDT in bytes.===
%% @end
%%-------------------------------------------------------------------------

-spec byte_size(Dispatcher :: cloudi_service:dispatcher(),
                State :: state()) ->
    non_neg_integer().

byte_size(Dispatcher,
          #cloudi_crdt{word_size = WordSize} = State)
    when is_pid(Dispatcher) ->
    cloudi_x_erlang_term:byte_size(State, WordSize).

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec clear(Dispatcher :: cloudi_service:dispatcher(),
            State :: state()) ->
    state().

clear(Dispatcher, State) ->
    clear_id(Dispatcher, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear a key in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec clear(Dispatcher :: cloudi_service:dispatcher(),
            Key :: key(),
            State :: state()) ->
    state().

clear(Dispatcher, Key, State) ->
    clear_id(Dispatcher, Key, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec clear_id(Dispatcher :: cloudi_service:dispatcher(),
               Id :: event_id(),
               State :: state()) ->
    state().

clear_id(Dispatcher, Id, State)
    when is_pid(Dispatcher) ->
    event_local({clear_all, Id}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear a key in the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec clear_id(Dispatcher :: cloudi_service:dispatcher(),
               Key :: key(),
               Id :: event_id(),
               State :: state()) ->
    state().

clear_id(Dispatcher, Key, Id, State)
    when is_pid(Dispatcher) ->
    event_local({clear, Id, Key}, State, Dispatcher).

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
    decr_id(Dispatcher, Key, undefined, State).

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

decr(Dispatcher, Key, Value, State) ->
    decr_id(Dispatcher, Key, Value, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decrement a numerical value by 1 in the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec decr_id(Dispatcher :: cloudi_service:dispatcher(),
              Key :: key(),
              Id :: event_id(),
              State :: state()) ->
    state().

decr_id(Dispatcher, Key, Id, State) ->
    decr_id(Dispatcher, Key, 1, Id, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decrement a numerical value in the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec decr_id(Dispatcher :: cloudi_service:dispatcher(),
              Key :: key(),
              Value :: number(),
              Id :: event_id(),
              State :: state()) ->
    state().

decr_id(Dispatcher, Key, Value, Id, State)
    when is_pid(Dispatcher) ->
    event_local({decr, Id, Key, Value}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to events from the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec events_subscribe(Dispatcher :: cloudi_service:dispatcher(),
                       Key :: key(),
                       State :: state()) ->
    state().

events_subscribe(Dispatcher, Key, State) ->
    events_subscribe(Dispatcher, Key, ?EVENT_TYPES, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to specific events from the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec events_subscribe(Dispatcher :: cloudi_service:dispatcher(),
                       Key :: key(),
                       EventTypes :: list(event_type()),
                       State :: state()) ->
    state().

events_subscribe(Dispatcher, Key, EventTypes,
                 #cloudi_crdt{events = Events} = State)
    when is_pid(Dispatcher) ->
    true = cloudi_lists:member_all(EventTypes, ?EVENT_TYPES),
    State#cloudi_crdt{events = maps:put(Key, lists:usort(EventTypes), Events)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscriptions on all keys to specific events from the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec events_subscriptions(Dispatcher :: cloudi_service:dispatcher(),
                           EventTypes :: list(event_type()),
                           State :: state()) ->
    state().

events_subscriptions(Dispatcher, EventTypes, State)
    when is_pid(Dispatcher) ->
    true = cloudi_lists:member_all(EventTypes, ?EVENT_TYPES),
    State#cloudi_crdt{events_any = lists:usort(EventTypes)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear all event subscriptions from the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec events_clear(Dispatcher :: cloudi_service:dispatcher(),
                   State :: state()) ->
    state().

events_clear(Dispatcher, State)
    when is_pid(Dispatcher) ->
    State#cloudi_crdt{events = #{},
                      events_any = []}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Clear a subscription to events from the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec events_clear(Dispatcher :: cloudi_service:dispatcher(),
                   Key :: key(),
                   State :: state()) ->
    state().

events_clear(Dispatcher, Key,
             #cloudi_crdt{events = Events} = State)
    when is_pid(Dispatcher) ->
    State#cloudi_crdt{events = maps:remove(Key, Events)}.

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
        #return_async_active{response = {state,
                                         NodeIdRemote,
                                         VClockRemote, VClocksRemote,
                                         POLogModeRemote,
                                         POLogRemote, DataRemote}}
        when Result == ok ->
            {ok, bootstrap_update_finish(NodeIdRemote,
                                         VClockRemote, VClocksRemote,
                                         POLogModeRemote,
                                         POLogRemote, DataRemote,
                                         StateNew)};
        _ ->
            {Result, StateNew}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Handle a CRDT service request.===
%% Must be called from the `cloudi_service_handle_request/11' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec handle_request(RequestType :: cloudi_service:request_type(),
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

handle_request(RequestType, ServiceNameFull, ServiceNameFull,
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
    ok = cloudi_service:return_nothrow(Dispatcher, RequestType,
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
    incr_id(Dispatcher, Key, 1, undefined, State).

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

incr(Dispatcher, Key, Value, State) ->
    incr_id(Dispatcher, Key, Value, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment a numerical value by 1 in the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec incr_id(Dispatcher :: cloudi_service:dispatcher(),
              Key :: key(),
              Id :: event_id(),
              State :: state()) ->
    state().

incr_id(Dispatcher, Key, Id, State) ->
    incr_id(Dispatcher, Key, 1, Id, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment a numerical value in the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec incr_id(Dispatcher :: cloudi_service:dispatcher(),
              Key :: key(),
              Value :: number(),
              Id :: event_id(),
              State :: state()) ->
    state().

incr_id(Dispatcher, Key, Value, Id, State)
    when is_pid(Dispatcher) ->
    event_local({incr, Id, Key, Value}, State, Dispatcher).

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
        {initial_data_function,         ?DEFAULT_INITIAL_DATA_FUNCTION},
        {clean_vclocks,                 ?DEFAULT_CLEAN_VCLOCKS},
        {clean_vclocks_failure,         ?DEFAULT_CLEAN_VCLOCKS_FAILURE},
        {retry,                         ?DEFAULT_RETRY},
        {retry_delay,                   ?DEFAULT_RETRY_DELAY},
        {timeout_default,               ?DEFAULT_TIMEOUT_DEFAULT},
        {priority_default,              ?DEFAULT_PRIORITY_DEFAULT},
        {priority_default_offset,       ?DEFAULT_PRIORITY_DEFAULT_OFFSET}],
    [ServiceName, InitialDataF, CleanIntervalSeconds, CleanFailure,
     Retry, RetryDelay, TimeoutDefault,
     PriorityDefault0, PriorityDefaultOffset] =
        cloudi_proplists:take_values(Defaults, Options),
    true = is_list(ServiceName) andalso is_integer(hd(ServiceName)),
    Prefix = cloudi_service:prefix(Dispatcher),
    ServiceNameFull = Prefix ++ ServiceName,
    false = cloudi_x_trie:is_pattern2(ServiceNameFull),
    true = cloudi_service:process_count_min(Dispatcher) > 1,
    true = (InitialDataF =:= undefined) orelse
           is_function(InitialDataF, 1),
    true = is_integer(CleanIntervalSeconds) andalso
           (CleanIntervalSeconds >= 1) andalso
           (CleanIntervalSeconds =< 4294967),
    true = is_number(CleanFailure) andalso
           (CleanFailure > 0) andalso (CleanFailure =< 100),
    true = not ((PriorityDefault0 /= undefined) andalso
                (PriorityDefaultOffset /= undefined)),
    PriorityDefaultN = if
        PriorityDefaultOffset =:= undefined ->
            PriorityDefault0;
        is_integer(PriorityDefaultOffset),
        PriorityDefaultOffset >= ?PRIORITY_HIGHER_OFFSET,
        PriorityDefaultOffset =< ?PRIORITY_LOWER_OFFSET ->
            PriorityDefault1 = cloudi_service:priority_default(Dispatcher) +
                               PriorityDefaultOffset,
            true = (PriorityDefault1 >= ?PRIORITY_HIGH) andalso
                   (PriorityDefault1 =< ?PRIORITY_LOW),
            PriorityDefault1
    end,
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
                              {timeout_default, TimeoutDefault},
                              {priority_default, PriorityDefaultN},
                              {failures_source_die, true}]),

    ok = cloudi_service:subscribe(Dispatcher, ServiceName),
    WordSize = erlang:system_info(wordsize),
    #cloudi_crdt{service_name_full = ServiceNameFull,
                 initial_data_function = InitialDataF,
                 clean_vclocks_interval = CleanIntervalSeconds,
                 clean_vclocks_failure = CleanFailure,
                 queue = Queue,
                 word_size = WordSize,
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

put(Dispatcher, Key, Value, State) ->
    put_id(Dispatcher, Key, Value, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Put a value into the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec put_id(Dispatcher :: cloudi_service:dispatcher(),
             Key :: key(),
             Value :: value(),
             Id :: event_id(),
             State :: state()) ->
    state().

put_id(Dispatcher, Key, Value, Id, State)
    when is_pid(Dispatcher) ->
    event_local({put, Id, Key, Value}, State, Dispatcher).

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
%% for every CloudI service process that shares this CloudI CRDT.
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

update(Dispatcher, Key, Module, Function, State) ->
    update_id(Dispatcher, Key,
              Module, Function, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value iff it exists in the CloudI CRDT.===
%% Function Module:Function/2 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
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

update(Dispatcher, Key, Module, Function, Argument1, State) ->
    update_id(Dispatcher, Key,
              Module, Function, Argument1, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value iff it exists in the CloudI CRDT with an event_id.===
%% Function Module:Function/1 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update_id(Dispatcher :: cloudi_service:dispatcher(),
                Key :: key(),
                Module :: module(),
                Function :: atom(),
                Id :: event_id(),
                State :: state()) ->
    state().

update_id(Dispatcher, Key, Module, Function, Id, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 1),
    event_local({update, Id, Key,
                 ModuleVersion, Module, Function},
                State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value iff it exists in the CloudI CRDT with an event_id.===
%% Function Module:Function/2 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update_id(Dispatcher :: cloudi_service:dispatcher(),
                Key :: key(),
                Module :: module(),
                Function :: atom(),
                Argument1 :: any(),
                Id :: event_id(),
                State :: state()) ->
    state().

update_id(Dispatcher, Key, Module, Function, Argument1, Id, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 2),
    event_local({update, Id, Key,
                 ModuleVersion, Module, Function, Argument1},
                State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or assign a value in the CloudI CRDT.===
%% Function Module:Function/1 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update_assign(Dispatcher :: cloudi_service:dispatcher(),
                    Key :: key(),
                    Value :: value(),
                    Module :: module(),
                    Function :: atom(),
                    State :: state()) ->
    state().

update_assign(Dispatcher, Key, Value, Module, Function, State) ->
    update_assign_id(Dispatcher, Key, Value,
                     Module, Function, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or assign a value in the CloudI CRDT.===
%% Function Module:Function/2 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update_assign(Dispatcher :: cloudi_service:dispatcher(),
                    Key :: key(),
                    Value :: value(),
                    Module :: module(),
                    Function :: atom(),
                    Argument1 :: any(),
                    State :: state()) ->
    state().

update_assign(Dispatcher, Key, Value, Module, Function, Argument1, State) ->
    update_assign_id(Dispatcher, Key, Value,
                     Module, Function, Argument1, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or assign a value in the CloudI CRDT with an event_id.===
%% Function Module:Function/1 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update_assign_id(Dispatcher :: cloudi_service:dispatcher(),
                       Key :: key(),
                       Value :: value(),
                       Module :: module(),
                       Function :: atom(),
                       Id :: event_id(),
                       State :: state()) ->
    state().

update_assign_id(Dispatcher, Key, Value, Module, Function, Id, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 1),
    event_local({update_assign, Id, Key, Value,
                 ModuleVersion, Module, Function},
                State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or assign a value in the CloudI CRDT with an event_id.===
%% Function Module:Function/2 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% @end
%%-------------------------------------------------------------------------

-spec update_assign_id(Dispatcher :: cloudi_service:dispatcher(),
                       Key :: key(),
                       Value :: value(),
                       Module :: module(),
                       Function :: atom(),
                       Argument1 :: any(),
                       Id :: event_id(),
                       State :: state()) ->
    state().

update_assign_id(Dispatcher, Key, Value, Module, Function, Argument1, Id, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 2),
    event_local({update_assign, Id, Key, Value,
                 ModuleVersion, Module, Function, Argument1},
                State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or clear the value in the CloudI CRDT.===
%% Function Module:Function/1 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% To clear the value, the function must return undefined.
%% @end
%%-------------------------------------------------------------------------

-spec update_clear(Dispatcher :: cloudi_service:dispatcher(),
                   Key :: key(),
                   Module :: module(),
                   Function :: atom(),
                   State :: state()) ->
    state().

update_clear(Dispatcher, Key, Module, Function, State) ->
    update_clear_id(Dispatcher, Key,
                    Module, Function, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or clear the value in the CloudI CRDT.===
%% Function Module:Function/2 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% To clear the value, the function must return undefined.
%% @end
%%-------------------------------------------------------------------------

-spec update_clear(Dispatcher :: cloudi_service:dispatcher(),
                   Key :: key(),
                   Module :: module(),
                   Function :: atom(),
                   Argument1 :: any(),
                   State :: state()) ->
    state().

update_clear(Dispatcher, Key, Module, Function, Argument1, State) ->
    update_clear_id(Dispatcher, Key,
                    Module, Function, Argument1, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or clear the value in the CloudI CRDT with an event_id.===
%% Function Module:Function/1 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% To clear the value, the function must return undefined.
%% @end
%%-------------------------------------------------------------------------

-spec update_clear_id(Dispatcher :: cloudi_service:dispatcher(),
                      Key :: key(),
                      Module :: module(),
                      Function :: atom(),
                      Id :: event_id(),
                      State :: state()) ->
    state().

update_clear_id(Dispatcher, Key, Module, Function, Id, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 1),
    event_local({update_clear, Id, Key,
                 ModuleVersion, Module, Function},
                State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value or clear the value in the CloudI CRDT with an event_id.===
%% Function Module:Function/2 must exist with the same version
%% for every CloudI service process that shares this CloudI CRDT.
%% If the function does not execute to return the same result
%% (when given the same value) for each instance of the CloudI CRDT,
%% it can create inconsistencies in the Erlang map that is used for
%% all read operations
%% (inconsistencies which would only be resolvable manually).
%% To clear the value, the function must return undefined.
%% @end
%%-------------------------------------------------------------------------

-spec update_clear_id(Dispatcher :: cloudi_service:dispatcher(),
                      Key :: key(),
                      Module :: module(),
                      Function :: atom(),
                      Argument1 :: any(),
                      Id :: event_id(),
                      State :: state()) ->
    state().

update_clear_id(Dispatcher, Key, Module, Function, Argument1, Id, State)
    when is_pid(Dispatcher) ->
    ModuleVersion = update_local_valid(Module, Function, 2),
    event_local({update_clear, Id, Key,
                 ModuleVersion, Module, Function, Argument1},
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
    zero_id(Dispatcher, Key, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Put a zero value in the CloudI CRDT with an event_id.===
%% @end
%%-------------------------------------------------------------------------

-spec zero_id(Dispatcher :: cloudi_service:dispatcher(),
              Key :: key(),
              Id :: event_id(),
              State :: state()) ->
    state().

zero_id(Dispatcher, Key, Id, State) ->
    put_id(Dispatcher, Key, 0, Id, State).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec node_id(Service :: cloudi_service:source()) ->
    node_id().

node_id(Service)
    when is_pid(Service) ->
    % n.b., The Erlang Term Binary Format for an Erlang pid (NEW_PID_EXT)
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
    {{vclock, node_id(), vclock()} | vclock_updated, state()}.

event_remote(NodeIdRemote, VClockRemote, Operation,
             #cloudi_crdt{service_name_full = ServiceNameFull,
                          node_id = NodeId,
                          polog_mode = bootstrap} = State, Dispatcher) ->
    case bootstrap_local_check(NodeIdRemote, VClockRemote, Operation,
                               State, Dispatcher) of
        {true, #cloudi_crdt{queue = Queue,
                            vclock = VClock0} = StateNew} ->
            {ok, QueueNew} = cloudi_queue:
                             mcast(Dispatcher, ServiceNameFull,
                                   {vclock, NodeId, VClock0}, Queue),
            VClockN = vclock_increment(NodeId, VClock0), % send
            {{vclock, NodeId, VClockN},
             StateNew#cloudi_crdt{queue = QueueNew,
                                  vclock = VClockN}};
        {false, StateNew} ->
            % block NodeIdRemote by not incrementing the vclock() until
            % after the bootstrap POLogMode is checked
            {vclock_updated, StateNew}
    end;
event_remote(NodeIdRemote, VClockRemote0, Operation,
             #cloudi_crdt{node_id = NodeId,
                          node_ids = NodeIds,
                          vclock = VClock0,
                          vclocks = VClocks,
                          polog_mode = normal,
                          polog = POLog,
                          data = Data,
                          events = Events,
                          events_any = EventsAny} = State, _) ->
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
    {POLogNew, DataNew} = polog_stable(POLogNext, Data,
                                       VClockMin, Events, EventsAny, NodeId),
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
                                data = Data,
                                events = Events,
                                events_any = EventsAny} = State,
                   Dispatcher) ->
    % Update the vclock() from the local operation broadcast response
    % that was provided by event_remote/4.

    VClockRemoteN = vclock_current(NodeIds, VClockRemote0),
    VClock1 = vclock_merge(VClockRemoteN,
                           vclock_increment(NodeId, VClock0)), % receive
    {QueueNew, VClockN} = case cloudi_queue:size(Dispatcher, Queue) of
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
    {POLogNew, DataNew} = polog_stable(POLog, Data,
                                       VClockMin, Events, EventsAny, NodeId),
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
                                 data = Data,
                                 events = Events,
                                 events_any = EventsAny} = State,
                    Dispatcher) ->
    % The vclock() broadcasted from event_local_vclock/4 updates the
    % the remote CloudI service process here.  The updated vclock() is
    % broadcasted if no other operations are being sent, to ensure the
    % the operation takes effect on all CloudI service processes as
    % quick as possible.

    VClockRemoteN = vclock_current(NodeIds, VClockRemote0),
    VClock1 = vclock_merge(VClockRemoteN,
                           vclock_increment(NodeId, VClock0)), % receive
    {QueueNew, VClockN} = case cloudi_queue:size(Dispatcher, Queue) of
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
    {POLogNew, DataNew} = polog_stable(POLog, Data,
                                       VClockMin, Events, EventsAny, NodeId),
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
                                        data = Data,
                                        events = Events,
                                        events_any = EventsAny} = State) ->
    % Update the vclock() from the local vclock() broadcast response
    % that was provided by event_remote_vclock/4.

    VClockRemoteN = vclock_current(NodeIds, VClockRemote0),
    VClockN = vclock_merge(VClockRemoteN,
                           vclock_increment(NodeId, VClock0)), % receive
    VClocksNew = vclocks_update(NodeId, VClockN,
                                vclocks_update(NodeIdRemote, VClockRemoteN,
                                               VClocks)),
    VClockMin = vclocks_minimum(VClocksNew),
    {POLogNew, DataNew} = polog_stable(POLog, Data,
                                       VClockMin, Events, EventsAny, NodeId),
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

-spec bootstrap_local_check(NodeIdRemote :: node_id(),
                            VClockRemote :: vclock(),
                            Operation :: operation_write(),
                            State :: state(),
                            Dispatcher :: cloudi_service:dispatcher()) ->
    {boolean(), state()}.

bootstrap_local_check(NodeIdRemote, VClockRemote, Operation,
                      #cloudi_crdt{service_name_full = ServiceNameFull,
                                   node_id = NodeId,
                                   polog_mode = bootstrap,
                                   bootstrap_node_id = BootstrapNodeId} = State,
                      Dispatcher) ->
    StateNext = bootstrap_local(NodeIdRemote, VClockRemote, State),
    #cloudi_crdt{node_ids = NodeIdsOld,
                 vclock = VClock0,
                 polog = POLog} = StateNext,
    POLogNew = polog_effect(Operation, VClock0, POLog),
    VClockN = vclock_increment(NodeId, VClock0), % event
    StateNew = StateNext#cloudi_crdt{vclock = VClockN,
                                     polog = POLogNew},
    if
        BootstrapNodeId =:= undefined ->
            case cloudi_service:get_pids(Dispatcher,
                                         ServiceNameFull, undefined) of
                {ok, [_ | _] = PatternPids} ->
                    NodeIdsNew = lists:foldl(fun({_, Pid}, NodeIdsNext) ->
                        lists:umerge(NodeIdsNext, [node_id(Pid)])
                    end, [NodeId], PatternPids),
                    BootstrapUpdate = (NodeIdsNew -- NodeIdsOld == []) andalso
                        lists:member(NodeIdRemote, NodeIdsNew),
                    if
                        BootstrapUpdate =:= true ->
                            {false,
                             bootstrap_update_start(PatternPids, NodeIdRemote,
                                                    StateNew#cloudi_crdt{
                                                        node_ids = NodeIdsNew},
                                                    Dispatcher)};
                        BootstrapUpdate =:= false ->
                            % send vclock() updates to allow the
                            % BootstrapUpdate to become true after the updates
                            {true, StateNew}
                    end;
                {error, timeout} ->
                    ?LOG_WARN("get_pids timeout", []),
                    {false, StateNew}
            end;
        true ->
            {false, StateNew}
    end.

-spec bootstrap_update_start(PatternPids :: list(cloudi_service:pattern_pid()),
                             BootstrapNodeId :: node_id(),
                             State :: state(),
                             Dispatcher :: cloudi_service:dispatcher()) ->
    state().

bootstrap_update_start(PatternPids, BootstrapNodeId, State, Dispatcher) ->
    bootstrap_update_start(PatternPids, 0, BootstrapNodeId, State, Dispatcher).

bootstrap_update_start([], Count, BootstrapNodeId, State, _) ->
    State#cloudi_crdt{bootstrap_node_id = BootstrapNodeId,
                      bootstrap_states = [],
                      bootstrap_requests = Count};
bootstrap_update_start([PatternPid | PatternPids], Count, BootstrapNodeId,
                       #cloudi_crdt{service_name_full = ServiceNameFull,
                                    queue = Queue} = State, Dispatcher) ->
    {ok, QueueNew} = cloudi_queue:send(Dispatcher, ServiceNameFull, <<>>,
                                       state, undefined, undefined,
                                       PatternPid, Queue),
    bootstrap_update_start(PatternPids, Count + 1, BootstrapNodeId,
                           State#cloudi_crdt{queue = QueueNew}, Dispatcher).

-spec bootstrap_update_finish(NodeIdRemote :: node_id(),
                              VClockRemote :: vclock(),
                              VClocksRemote :: vclocks(),
                              POLogModeRemote :: polog_mode(),
                              POLogRemote :: polog(),
                              DataRemote :: data(),
                              State :: state()) ->
    state().

bootstrap_update_finish(NodeIdRemote,
                        VClockRemote,
                        VClocksRemote,
                        POLogModeRemote,
                        POLogRemote,
                        DataRemote,
                        #cloudi_crdt{node_ids = NodeIds,
                                     polog_mode = bootstrap,
                                     bootstrap_states = BootstrapStates,
                                     bootstrap_requests = Count} = State) ->
    BootstrapStatesNew = [{vclock_average(vclock_current(NodeIds,
                                                         VClockRemote)),
                           NodeIdRemote, VClockRemote, VClocksRemote,
                           POLogModeRemote, POLogRemote, DataRemote} |
                          BootstrapStates],
    bootstrap_update_finished(State#cloudi_crdt{
                                  bootstrap_states = BootstrapStatesNew,
                                  bootstrap_requests = Count - 1}).

-spec bootstrap_update_finished(State :: state()) ->
    state().

bootstrap_update_finished(#cloudi_crdt{initial_data_function = InitialDataF,
                                       clean_vclocks_interval = CleanInterval,
                                       node_id = NodeId,
                                       node_ids = NodeIds,
                                       vclock = VClock0,
                                       bootstrap_node_id = BootstrapNodeId,
                                       bootstrap_states = BootstrapStates,
                                       bootstrap_requests = 0} = State) ->
    StateNext = State#cloudi_crdt{bootstrap_node_id = undefined,
                                  bootstrap_states = []},
    case bootstrap_update_result(BootstrapNodeId,
                                 BootstrapStates,
                                 vclock_average(vclock_current(NodeIds,
                                                               VClock0))) of
        bootstrap_done ->
            StateNew = clean_vclocks_store(StateNext),
            ok = clean_vclocks_send(CleanInterval * 1000, NodeId),
            StateNew#cloudi_crdt{polog_mode = normal};
        {ok, VClockRemote, VClocks, POLog, Data} ->
            % Use CRDT state from the NodeIdRemoteBlocked process.
            if
                InitialDataF =:= undefined ->
                    ok;
                is_function(InitialDataF, 1) ->
                    _ = InitialDataF(Data),
                    ok
            end,
            VClockN = vclock_merge(VClockRemote,
                                   vclock_increment(NodeId, VClock0)), % event
            StateNew = clean_vclocks_store(StateNext#cloudi_crdt{
                                               vclock = VClockN,
                                               vclocks = VClocks,
                                               polog = POLog,
                                               data = Data}),
            ok = clean_vclocks_send(CleanInterval * 1000, NodeId),
            StateNew#cloudi_crdt{polog_mode = normal};
        {error, _} ->
            StateNext
    end;
bootstrap_update_finished(State) ->
    State.

-spec bootstrap_update_result(BootstrapNodeId :: node_id(),
                              BootstrapStates :: list(bootstrap_state()),
                              VClockAvg :: float()) ->
    bootstrap_done |
    {ok, vclock(), vclocks(), polog(), data()} |
    {error, update_invalid}.

bootstrap_update_result(BootstrapNodeId, BootstrapStates, VClockAvg) ->
    VClockAvgL = [VClockAvg |
                  [VClockAvgRemote
                   || {VClockAvgRemote, _, _, _, _, _, _} <- BootstrapStates]],
    AllNewProcesses = lists:all(fun(VClockAvgValue) ->
        % The comparison here uses an arbitrary number based on testing
        % to determine what a new process really is.
        VClockAvgValue < 30
    end, VClockAvgL),
    if
        AllNewProcesses =:= true ->
            % All the CloudI service processes have been started recently
            % based on all the vclock() integers (so all are effectively
            % in the bootstrap POLogMode at the same time and will find
            % agreement on the NodeIds here).
            bootstrap_done;
        AllNewProcesses =:= false ->
            % The CRDT bootstrap POLogMode is not due to an initial startup
            % of a CloudI service, but is instead due to a restart or the
            % start of a new CloudI service instance.
            % (assumes a normal distribution of vclock() averages exists)
            Count = length(BootstrapStates),
            VClockAllAvg = lists:sum(VClockAvgL) / (Count + 1),
            VClockAllAvgStdDev = math:pow(lists:foldl(fun(VClockAvgValue,
                                                          SqsSum) ->
                Diff = VClockAvgValue - VClockAllAvg,
                Diff * Diff + SqsSum
            end, 0, VClockAvgL) / Count, 0.5), % sample stddev

            % 50% is 0.67448975019608171 * stddev
            VClockAllAvgThreshold = VClockAllAvg -
                VClockAllAvgStdDev * 0.67448975019608171,
            case lists:keyfind(BootstrapNodeId, 2, BootstrapStates) of
                {VClockAvgRemote, BootstrapNodeId,
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
                   VClockMin :: vclock(),
                   Events :: events(),
                   EventsAny :: list(event_type()),
                   NodeId :: node_id()) ->
    {polog(), data()}.

polog_stable(POLog, Data, VClockMin, Events, EventsAny, NodeId) ->
    polog_stable(lists:reverse(POLog), [], Data,
                 VClockMin, Events, EventsAny, NodeId).

polog_stable([], POLogNew, Data, _, _, _, _) ->
    {POLogNew, Data};
polog_stable([{VClock, Operation} = POLogValue | POLogOld], POLogNew, Data,
             VClockMin, Events, EventsAny, {_, Service} = NodeId) ->
    case vclock_less_than(VClock, VClockMin) of
        true ->
            polog_stable(POLogOld, POLogNew,
                         write(Operation, Data, Events, EventsAny, Service),
                         VClockMin, Events, EventsAny, NodeId);
        false ->
            polog_stable(POLogOld, [POLogValue | POLogNew], Data,
                         VClockMin, Events, EventsAny, NodeId)
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

polog_redundancy_relation({assign, _, _, _}, _, POLog) ->
    % assign can not be determined to be redundant because its
    % effect is only determined once state is consistent
    % (any number of put or clear operations may compete to determine
    %  whether an assign operation may succeed at a later point in time).
    {add, POLog};
polog_redundancy_relation({incr, _, _, _}, _, POLog) ->
    % Both incr and decr only mutate existing data and are unable to be
    % redundant, unless there is an incr/decr pair that contain the same
    % Key and Value (or some combination is equivalent to this).
    % The occurrence of redundant incr/decr will be infrequent and it is
    % best to avoid the extra processing a check would require.
    {add, POLog};
polog_redundancy_relation({decr, _, _, _}, _, POLog) ->
    {add, POLog};
polog_redundancy_relation({update, _, _, _, _, _}, _, POLog) ->
    % update may contain any operation that operates on a value,
    % if a value exists, so it is unable to be redundant
    % (similar to incr and decr, but more generic).
    {add, POLog};
polog_redundancy_relation({update, _, _, _, _, _, _}, _, POLog) ->
    {add, POLog};
polog_redundancy_relation({update_assign, _, _, _, _, _, _}, _, POLog) ->
    % update_assign is a combination of update and assign, it is not a put
    % that replaces a value without depending on previous values, so it is
    % unable to be redundant
    % (similar to update and assign when used separately).
    {add, POLog};
polog_redundancy_relation({update_assign, _, _, _, _, _, _, _}, _, POLog) ->
    {add, POLog};
polog_redundancy_relation({update_clear, _, _, _, _, _}, _, POLog) ->
    % update_clear is a combination of update and clear, it is not a put
    % that replaces a value without depending on previous values, so it is
    % unable to be redundant
    {add, POLog};
polog_redundancy_relation({update_clear, _, _, _, _, _, _}, _, POLog) ->
    {add, POLog};
polog_redundancy_relation({put, _, Key, _}, VClock, POLog) ->
    % only removes the first redundant operation to prevent memory growth
    polog_redundancy_relation_key(POLog, [], Key, VClock);
polog_redundancy_relation({clear, _, Key}, VClock, POLog) ->
    % only removes the first redundant operation to prevent memory growth
    polog_redundancy_relation_key(POLog, [], Key, VClock);
polog_redundancy_relation({clear_all, _}, VClock, POLog) ->
    polog_redundancy_relation_clear_all(POLog, [], VClock).

polog_redundancy_relation_key([], POLog, _, _) ->
    {add, lists:reverse(POLog)};
polog_redundancy_relation_key([{VClock,
                                {put, _, Key, _}} | POLogWithout] = POLogWith,
                              POLog, Key, VClockNow) ->
    polog_redundancy_relation_conflict_resolve(POLogWith, POLogWithout, POLog,
                                               VClock, VClockNow);
polog_redundancy_relation_key([{VClock,
                                {clear, _, Key}} | POLogWithout] = POLogWith,
                              POLog, Key, VClockNow) ->
    polog_redundancy_relation_conflict_resolve(POLogWith, POLogWithout, POLog,
                                               VClock, VClockNow);
polog_redundancy_relation_key([POLogValue | POLogWithout],
                              POLog, Key, VClockNow) ->
    polog_redundancy_relation_key(POLogWithout,
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
            Data :: data(),
            Events :: events(),
            EventsAny :: list(event_type()),
            Service :: cloudi_service:source()) ->
    DataNew :: data().

write({assign, Id, Key, ValueNew},
      Data, Events, EventsAny, Service) ->
    event(assign, Id, Key, [Data, ValueNew],
          Events, EventsAny, Service),
    maps:update_with(Key, fun(ValueOld) ->
        ValueOld
    end, ValueNew, Data);
write({incr, Id, Key, Value},
      Data, Events, EventsAny, Service) ->
    try maps:update_with(Key, fun(ValueOld) ->
            if
                is_number(ValueOld) ->
                    ValueNew = ValueOld + Value,
                    event(incr, Id, Key, [ValueOld, ValueNew],
                          Events, EventsAny, Service),
                    ValueNew;
                true ->
                    ValueOld
            end
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({decr, Id, Key, Value},
      Data, Events, EventsAny, Service) ->
    try maps:update_with(Key, fun(ValueOld) ->
            if
                is_number(ValueOld) ->
                    ValueNew = ValueOld - Value,
                    event(decr, Id, Key, [ValueOld, ValueNew],
                          Events, EventsAny, Service),
                    ValueNew;
                true ->
                    ValueOld
            end
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({update, Id, Key, ModuleVersion, Module, Function},
      Data, Events, EventsAny, Service) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 1),
    try maps:update_with(Key, fun(ValueOld) ->
            ValueNew = Module:Function(ValueOld),
            event(update, Id, Key, [ValueOld, ValueNew],
                  Events, EventsAny, Service),
            ValueNew
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({update, Id, Key, ModuleVersion, Module, Function, Argument1},
      Data, Events, EventsAny, Service) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 2),
    try maps:update_with(Key, fun(ValueOld) ->
            ValueNew = Module:Function(Argument1, ValueOld),
            event(update, Id, Key, [ValueOld, ValueNew],
                  Events, EventsAny, Service),
            ValueNew
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({update_assign, Id, Key, Value,
       ModuleVersion, Module, Function},
      Data, Events, EventsAny, Service) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 1),
    try maps:update_with(Key, fun(ValueOld) ->
            ValueNew = Module:Function(ValueOld),
            event(update, Id, Key, [ValueOld, ValueNew],
                  Events, EventsAny, Service),
            ValueNew
        end, Data)
    catch
        error:{badkey, Key} ->
            event(assign, Id, Key, [Value],
                  Events, EventsAny, Service),
            maps:put(Key, Value, Data)
    end;
write({update_assign, Id, Key, Value,
       ModuleVersion, Module, Function, Argument1},
      Data, Events, EventsAny, Service) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 2),
    try maps:update_with(Key, fun(ValueOld) ->
            ValueNew = Module:Function(Argument1, ValueOld),
            event(update, Id, Key, [ValueOld, ValueNew],
                  Events, EventsAny, Service),
            ValueNew
        end, Data)
    catch
        error:{badkey, Key} ->
            event(assign, Id, Key, [Value],
                  Events, EventsAny, Service),
            maps:put(Key, Value, Data)
    end;
write({update_clear, Id, Key,
       ModuleVersion, Module, Function},
      Data, Events, EventsAny, Service) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 1),
    try maps:update_with(Key, fun(ValueOld) ->
            case Module:Function(ValueOld) of
                undefined ->
                    erlang:exit(clear);
                ValueNew ->
                    event(update, Id, Key, [ValueOld, ValueNew],
                          Events, EventsAny, Service),
                    ValueNew
            end
        end, Data)
    catch
        exit:clear ->
            event(clear, Id, Key, [Data],
                  Events, EventsAny, Service),
            maps:remove(Key, Data);
        error:{badkey, Key} ->
            Data
    end;
write({update_clear, Id, Key,
       ModuleVersion, Module, Function, Argument1},
      Data, Events, EventsAny, Service) ->
    ok = update_remote_valid(ModuleVersion, Module, Function, 2),
    try maps:update_with(Key, fun(ValueOld) ->
            case Module:Function(Argument1, ValueOld) of
                undefined ->
                    erlang:exit(clear);
                ValueNew ->
                    event(update, Id, Key, [ValueOld, ValueNew],
                          Events, EventsAny, Service),
                    ValueNew
            end
        end, Data)
    catch
        exit:clear ->
            event(clear, Id, Key, [Data],
                  Events, EventsAny, Service),
            maps:remove(Key, Data);
        error:{badkey, Key} ->
            Data
    end;
write({put, Id, Key, ValueNew},
      Data, Events, EventsAny, Service) ->
    event(put, Id, Key, [Data, ValueNew],
          Events, EventsAny, Service),
    maps:put(Key, ValueNew, Data);
write({clear, Id, Key},
      Data, Events, EventsAny, Service) ->
    event(clear, Id, Key, [Data],
          Events, EventsAny, Service),
    maps:remove(Key, Data);
write({clear_all, Id},
      Data, Events, EventsAny, Service) ->
    events(clear, Id, maps:keys(Data), [Data],
           Events, EventsAny, Service),
    maps:new().

-spec event(EventType :: event_type(),
            EventId :: event_id(),
            Key :: key(),
            EventData :: list(data() | value()),
            Events :: events(),
            EventsAny :: list(event_type()),
            Service :: cloudi_service:source()) ->
    ok.

event(EventType, EventId, Key, EventData, Events, EventsAny, Service) ->
    Send = case lists:member(EventType, EventsAny) of
        true ->
            true;
        false ->
            case maps:find(Key, Events) of
                {ok, EventTypes} ->
                    lists:member(EventType, EventTypes);
                error ->
                    false
            end
    end,
    if
        Send =:= true ->
            event_send(EventType, EventId, Key, EventData, Service);
        Send =:= false ->
            ok
    end,
    ok.

-spec events(EventType :: clear,
             EventId :: event_id(),
             Keys :: list(key()),
             EventData :: list(data()),
             Events :: events(),
             EventsAny :: list(event_type()),
             Service :: cloudi_service:source()) ->
    ok.

events(EventType, EventId, Keys, EventData, Events, EventsAny, Service) ->
    case lists:member(EventType, EventsAny) of
        true ->
            lists:foreach(fun(Key) ->
                event_send(EventType, EventId, Key, EventData, Service)
            end, Keys);
        false ->
            maps:fold(fun(Key, EventTypes, _) ->
                case lists:member(EventType, EventTypes) of
                    true ->
                        event_send(EventType, EventId, Key, EventData, Service);
                    false ->
                        ok
                end
            end, ok, maps:with(Keys, Events))
    end,
    ok.

-spec event_send(event_type(),
                 EventId :: event_id(),
                 Key :: key(),
                 EventData :: list(data() | value()),
                 Service :: cloudi_service:source()) ->
    any().

event_send(assign, EventId, Key, EventData, Service) ->
    case EventData of
        [ValueNew] ->
            Service ! #crdt_event{type = assign,
                                  id = EventId,
                                  key = Key,
                                  new = {value, ValueNew}};
        [Data, ValueNew] ->
            case maps:find(Key, Data) of
                {ok, _} ->
                    ok;
                error ->
                    Service ! #crdt_event{type = assign,
                                          id = EventId,
                                          key = Key,
                                          new = {value, ValueNew}}
            end
    end;
event_send(incr, EventId, Key, [ValueOld, ValueNew], Service) ->
    Service ! #crdt_event{type = incr,
                          id = EventId,
                          key = Key,
                          old = {value, ValueOld},
                          new = {value, ValueNew}};
event_send(decr, EventId, Key, [ValueOld, ValueNew], Service) ->
    Service ! #crdt_event{type = decr,
                          id = EventId,
                          key = Key,
                          old = {value, ValueOld},
                          new = {value, ValueNew}};
event_send(update, EventId, Key, [ValueOld, ValueNew], Service) ->
    Service ! #crdt_event{type = update,
                          id = EventId,
                          key = Key,
                          old = {value, ValueOld},
                          new = {value, ValueNew}};
event_send(put, EventId, Key, [Data, ValueNew], Service) ->
    Old = case maps:find(Key, Data) of
        {ok, Value} ->
            {value, Value};
        error ->
            undefined
    end,
    Service ! #crdt_event{type = put,
                          id = EventId,
                          key = Key,
                          old = Old,
                          new = {value, ValueNew}};
event_send(clear, EventId, Key, [Data], Service) ->
    case maps:find(Key, Data) of
        {ok, Value} ->
            Service ! #crdt_event{type = clear,
                                  id = EventId,
                                  key = Key,
                                  old = {value, Value}};
        error ->
            ok
    end.

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
                                 data = Data0,
                                 events = Events,
                                 events_any = EventsAny} = State) ->
    VClockN = vclock_current(NodeIds, VClock0),
    VClocks1 = vclocks_update(NodeId, VClockN, VClocks0),
    VClocksN = vclocks_current(NodeIds, VClocks1),
    POLog1 = [{vclock_current(NodeIds, VClockPOLog), Operation} ||
              {VClockPOLog, Operation} <- POLog0],
    VClockMin = vclocks_minimum(VClocksN),
    {POLogN, DataN} = polog_stable(POLog1, Data0,
                                   VClockMin, Events, EventsAny, NodeId),
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

