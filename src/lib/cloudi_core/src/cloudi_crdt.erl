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
%%% It is best to have services that utilize cloudi_crdt be configured with
%%% an "oldest" destination refresh method so a process restart may obtain
%%% existing state from the most stable of the peer processes.  The only
%%% cloudi_crdt function that may be called within cloudi_service_init/4
%%% is the new function.
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
%%% Lamport, Leslie. "Time, clocks, and the ordering of events in a
%%% distributed system". Communications of the ACM. 21 (7): 558–565. (1978)
%%% http://research.microsoft.com/en-us/um/people/lamport/pubs/time-clocks.pdf
%%%
%%% Mattern, Friedemann. "Virtual Time and Global States of
%%% Distributed Systems". Workshop on Parallel and Distributed
%%% Algorithms: pp. 215-226 (1988).
%%% http://homes.cs.washington.edu/~arvind/cs425/doc/mattern89virtual.pdf
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
-export([clear/2,
         clear/3,
         decr/3,
         decr/4,
         find/3,
         get/3,
         handle_info/3,
         handle_request/11,
         incr/3,
         incr/4,
         is_key/3,
         keys/2,
         new/2,
         new/3,
         put/4,
         zero/3]).

-type node_id() :: {node(), cloudi_service:source()}.
-type vclock() :: #{node_id() := non_neg_integer()}.
-type vclocks() :: #{node_id() := vclock()}.
-type operation_write() ::
    {incr,  Key :: any(), Value :: any()} |
    {decr,  Key :: any(), Value :: any()} |
    {put,   Key :: any(), Value :: any()} |
    {clear, Key :: any()} |
    clear_all.

% The POLog is ordered based on the receive order of the operations
% (newest operation (head) and older operations (tail)),
% with redundant operations resolved by the redundancy relation
% (which is data type specific, an Erlang map is used here)
-type polog() :: list({vclock(), operation_write()}).

% The POLog data type that contains consistent state (an Erlang map)
-type data() :: #{any() := any()}.

-record(cloudi_crdt,
    {
        service_name :: cloudi_service:service_name(),
        service_name_full :: cloudi_service:service_name(),
        remote_state_id :: cloudi_service:trans_id() | undefined,
        queue :: cloudi_queue:state(),
        node_id :: node_id(),
        vclock :: vclock(),
        vclocks = vclocks_new() :: vclocks(),
        polog = [] :: polog(),
        data = #{} :: data()
    }).

-record(cloudi_crdt_state,
    {
        vclock :: vclock(),
        vclocks :: vclocks(),
        polog :: polog(),
        data :: data()
    }).

-include("cloudi_service.hrl").

-define(DEFAULT_SERVICE_NAME,                    "crdt").
-define(DEFAULT_RETRY,                                0). % see below:
        % a retry doesn't count as a failure, until it fails completely
        % (i.e., hit the max retry count or send returns an error)
-define(DEFAULT_RETRY_DELAY,                          0). % milliseconds

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type options() ::
    list({service_name, string()} |
         {retry, non_neg_integer()} |
         {retry_delay, non_neg_integer()}).
-type state() :: #cloudi_crdt{}.
-export_type([options/0,
              state/0]).

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
            Key :: any(),
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
           Key :: any(),
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
           Key :: any(),
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
           Key :: any(),
           State :: state()) ->
    {{ok, Value :: any()}, state()} |
    {error, state()}.

find(Dispatcher, Key,
     #cloudi_crdt{data = Data} = State)
    when is_pid(Dispatcher) ->
    {read({find, Key}, Data), State}.
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Get a value from the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Dispatcher :: cloudi_service:dispatcher(),
          Key :: any(),
          State :: state()) ->
    {Value :: any(), state()}.

get(Dispatcher, Key,
    #cloudi_crdt{data = Data} = State)
    when is_pid(Dispatcher) ->
    {read({get, Key}, Data), State}.
    
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

handle_info(Request,
            #cloudi_crdt{service_name = ServiceName,
                         remote_state_id = RemoteStateId,
                         queue = Queue,
                         vclock = VClock} = State,
            Dispatcher) ->
    {Result, QueueNew} = cloudi_queue:handle_info(Request, Queue, Dispatcher),
    StateNew = State#cloudi_crdt{queue = QueueNew},
    case Request of
        #return_async_active{response = RemoteState,
                             trans_id = RemoteStateId}
        when Result == ignored ->
            #cloudi_crdt_state{vclock = VClockRemote,
                               vclocks = VClocksRemote,
                               polog = POLogRemote,
                               data = DataRemote} = RemoteState,
            VClockMerged = vclock_merge(VClock, VClockRemote),
            ok = cloudi_service:subscribe(Dispatcher, ServiceName),
            {ok, StateNew#cloudi_crdt{remote_state_id = undefined,
                                      vclock = VClockMerged,
                                      vclocks = VClocksRemote,
                                      polog = POLogRemote,
                                      data = DataRemote}};
        #timeout_async_active{trans_id = RemoteStateId}
        when Result == ignored ->
            % A timeout occurred after attempting to initialize with
            % remote data after the CloudI service process restarted.
            erlang:exit(remote_state_timeout),
            {{error, remote_state_timeout},
             StateNew#cloudi_crdt{remote_state_id = undefined}};
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
                            vclock = VClock,
                            vclocks = VClocks,
                            polog = POLog,
                            data = Data} = State,
               Dispatcher) ->
    {Response, StateNew} = case Request of
        state ->
            {#cloudi_crdt_state{vclock = VClock,
                                vclocks = VClocks,
                                polog = POLog,
                                data = Data}, State};
        {operation, NodeIdRemote, VClockRemote, Operation} ->
            event_remote(NodeIdRemote, VClockRemote, Operation, State);
        {vclock, NodeIdRemote, VClockRemote} ->
            event_remote_vclock(NodeIdRemote, VClockRemote,
                                State, Dispatcher);
        {vclock_updated, NodeIdRemote, VClockRemote} ->
            event_remote_vclock_updated(NodeIdRemote, VClockRemote, State)
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
           Key :: any(),
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
           Key :: any(),
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
             Key :: any(),
             State :: state()) ->
    {boolean(), state()}.

is_key(Dispatcher, Key,
       #cloudi_crdt{data = Data} = State)
    when is_pid(Dispatcher) ->
    {read({is_key, Key}, Data), State}.
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Get all keys in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec keys(Dispatcher :: cloudi_service:dispatcher(),
           State :: state()) ->
    {list(), state()}.

keys(Dispatcher,
     #cloudi_crdt{data = Data} = State)
    when is_pid(Dispatcher) ->
    {read(keys, Data), State}.
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Timeout :: cloudi_service:timeout_milliseconds()) ->
    state().

new(Dispatcher, Timeout) ->
    new(Dispatcher, Timeout, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Timeout :: cloudi_service:timeout_milliseconds(),
          Options :: options()) ->
    state().

new(Dispatcher, Timeout, Options)
    when is_pid(Dispatcher), is_list(Options) ->
    Defaults = [
        {service_name,                  ?DEFAULT_SERVICE_NAME},
        {retry,                         ?DEFAULT_RETRY},
        {retry_delay,                   ?DEFAULT_RETRY_DELAY}],
    [ServiceName, Retry, RetryDelay] =
        cloudi_proplists:take_values(Defaults, Options),
    true = is_list(ServiceName) andalso is_integer(hd(ServiceName)),
    Prefix = cloudi_service:prefix(Dispatcher),
    ServiceNameFull = Prefix ++ ServiceName,
    false = cloudi_x_trie:is_pattern(ServiceNameFull),
    Service = cloudi_service:self(Dispatcher),
    % CloudI CRDT service requests need to be ordered so that a retry
    % that occurs after a failed send does not allow a duplicate of an
    % operation to arrive after the operation has been removed from the POLog
    % (i.e., taken effect in the data type, the Erlang map).
    Queue = cloudi_queue:new([{retry, Retry},
                              {retry_delay, RetryDelay},
                              {ordered, true},
                              {failures_source_die, true}]),
    NodeId = node_id(Service),
    VClock0 = vclock_new(),
    VClockN = VClock0#{NodeId => 0},
    RemoteStateId = case cloudi_service:get_pid(Dispatcher,
                                                ServiceNameFull, limit_min) of
        {ok, PatternPid} ->
            case cloudi_service:send_async(Dispatcher, ServiceNameFull, <<>>,
                                           state, Timeout, undefined,
                                           PatternPid) of
                {ok, TransId} ->
                    TransId;
                {error, timeout} ->
                    undefined
            end;
        {error, timeout} ->
            undefined
    end,
    if
        RemoteStateId =:= undefined ->
            ok = cloudi_service:subscribe(Dispatcher, ServiceName);
        is_binary(RemoteStateId) ->
            % initialization is delayed until remote state is received
            ok
    end,
    #cloudi_crdt{service_name = ServiceName,
                 service_name_full = ServiceNameFull,
                 remote_state_id = RemoteStateId,
                 queue = Queue,
                 node_id = NodeId,
                 vclock = VClockN}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Put a value into the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Dispatcher :: cloudi_service:dispatcher(),
          Key :: any(),
          Value :: any(),
          State :: state()) ->
    state().

put(Dispatcher, Key, Value, State)
    when is_pid(Dispatcher) ->
    event_local({put, Key, Value}, State, Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a zero value in the CloudI CRDT.===
%% @end
%%-------------------------------------------------------------------------

-spec zero(Dispatcher :: cloudi_service:dispatcher(),
           Key :: any(),
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
                   State :: state()) ->
    {{vclock, node_id(), vclock()}, state()}.

event_remote(NodeIdRemote, VClockRemote, Operation,
             #cloudi_crdt{node_id = NodeId,
                          vclock = VClock0,
                          vclocks = VClocks,
                          polog = POLog,
                          data = Data} = State) ->
    % A remote write operation is received and added to the POLog
    % (from a CloudI service request sent by the broadcast in event_local/3).
    % The current vclock() is provided as a response for this
    % CloudI service request in the broadcast.

    VClock1 = vclock_merge(VClockRemote,
                           vclock_increment(NodeId, VClock0)), % receive
    POLogNext = polog_effect(Operation, VClock1, POLog),
    VClockN = vclock_increment(NodeId, VClock1), % event
    VClocksNew = vclocks_update(NodeId, VClockN,
                                vclocks_update(NodeIdRemote, VClockRemote,
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
                                queue = Queue,
                                node_id = NodeId,
                                vclock = VClock0,
                                vclocks = VClocks,
                                polog = POLog,
                                data = Data} = State,
                   Dispatcher) ->
    % Update the vclock() from the local operation broadcast response
    % that was provided by event_remote/4.

    VClock1 = vclock_merge(VClockRemote,
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
                                vclocks_update(NodeIdRemote, VClockRemote,
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
                    #cloudi_crdt{service_name_full = ServiceNameFull,
                                 queue = Queue,
                                 node_id = NodeId,
                                 vclock = VClock0,
                                 vclocks = VClocks,
                                 polog = POLog,
                                 data = Data} = State,
                    Dispatcher) ->
    % The vclock() broadcasted from event_local_vclock/4 updates the
    % the remote CloudI service process here.  The updated vclock() is
    % broadcasted if no other operations are being sent, to ensure the
    % the operation takes effect on all CloudI service processes as
    % quick as possible.

    VClock1 = vclock_merge(VClockRemote,
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
                                vclocks_update(NodeIdRemote, VClockRemote,
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

event_local_vclock_updated(NodeIdRemote, VClockRemote,
                           #cloudi_crdt{node_id = NodeId,
                                        vclock = VClock0,
                                        vclocks = VClocks,
                                        polog = POLog,
                                        data = Data} = State) ->
    % Update the vclock() from the local vclock() broadcast response
    % that was provided by event_remote_vclock/4.

    VClockN = vclock_merge(VClockRemote,
                           vclock_increment(NodeId, VClock0)), % receive
    VClocksNew = vclocks_update(NodeId, VClockN,
                                vclocks_update(NodeIdRemote, VClockRemote,
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

event_remote_vclock_updated(NodeIdRemote, VClockRemote, State) ->
    % The vclock() broadcasted from event_remote_vclock/4 updates the
    % the remote CloudI service process here.

    {vclock_updated,
     event_local_vclock_updated(NodeIdRemote, VClockRemote, State)}.

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

polog_redundancy_relation({incr, _, _}, _, POLog) ->
    % Both incr and decr only mutate existing data and are unable to be
    % redundant, unless there is an incr/decr pair that contain the same
    % Key and Value (or some combination is equivalent to this).
    % The occurrence of redundant incr/decr will be infrequent and it is
    % best to avoid the extra processing a check would require.
    {add, POLog};
polog_redundancy_relation({decr, _, _}, _, POLog) ->
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

read({find, Key}, Data) ->
    maps:find(Key, Data);
read({get, Key}, Data) ->
    maps:get(Key, Data);
read({is_key, Key}, Data) ->
    maps:is_key(Key, Data);
read(keys, Data) ->
    maps:keys(Data).

write({incr, Key, Value}, Data) ->
    try maps:update_with(Key, fun(OldValue) ->
            if
                is_number(OldValue) ->
                    OldValue + Value;
                true ->
                    OldValue
            end
        end, Data)
    catch
        error:{badkey, Key} ->
            Data
    end;
write({decr, Key, Value}, Data) ->
    try maps:update_with(Key, fun(OldValue) ->
            if
                is_number(OldValue) ->
                    OldValue - Value;
                true ->
                    OldValue
            end
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

