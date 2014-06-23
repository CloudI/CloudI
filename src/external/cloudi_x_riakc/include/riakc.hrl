%% -------------------------------------------------------------------
%%
%% riakc: protocol buffer client
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-define(PROTO_MAJOR, 1).
-define(PROTO_MINOR, 0).
-define(DEFAULT_TIMEOUT, 60000).
-define(FIRST_RECONNECT_INTERVAL, 100).
-define(MAX_RECONNECT_INTERVAL, 30000).

-type client_option()  :: queue_if_disconnected |
                          {queue_if_disconnected, boolean()} |
                          {connect_timeout, pos_integer()} |
                          auto_reconnect |
                          {auto_reconnect, boolean()}.
%% Options for starting or modifying the connection:
%% `queue_if_disconnected' when present or true will cause requests to
%% be queued while the connection is down. `auto_reconnect' when
%% present or true will automatically attempt to reconnect to the
%% server if the connection fails or is lost.
-type client_options() :: [client_option()]. %% A list of client options.
-type client_id() :: binary(). %% A client identifier, used for differentiating client processes
-type bucket() :: binary(). %% A bucket name.
-type key() :: binary(). %% A key name.
-type riakc_obj() :: riakc_obj:riakc_obj(). %% An object (bucket, key, metadata, value) stored in Riak.
-type req_id() :: non_neg_integer(). %% Request identifier for streaming requests.
-type server_prop() :: {node, binary()} | {server_version, binary()}. %% Server properties, as returned by the `get_server_info/1' call.
-type server_info() :: [server_prop()]. %% A response from the `get_server_info/1' call.
-type bucket_prop() :: {n_val, pos_integer()} | {allow_mult, boolean()}. %% Bucket property definitions.
-type bucket_props() :: [bucket_prop()]. %% Bucket properties
-type quorum() :: non_neg_integer() | one | all | quorum | default.  %% A quorum setting for get/put/delete requests.
-type read_quorum() :: {r, ReadQuorum::quorum()} |
                       {pr, PrimaryReadQuorum::quorum()}. %% Valid quorum options for get requests.
-type write_quorum() :: {w, WriteQuorum::quorum()} |
                        {dw, DurableWriteQuorum::quorum()} |
                        {pw, PrimaryWriteQuorum::quorum()}. %% Valid quorum options for write requests.
-type delete_option() :: delete_quorum()  |
                      {n_val, pos_integer()} |
                      {sloppy_quorum, boolean()}.
-type delete_quorum() :: read_quorum() |
                         write_quorum() |
                         {rw, ReadWriteQuorum::quorum()}. %% Valid quorum options for delete requests. Note that `rw' is deprecated in Riak 1.0 and later.
-type get_option() :: read_quorum() |
                      {if_modified, riakc_obj:vclock()} |
                      {notfound_ok, boolean()} |
                      {basic_quorum, boolean()} |
                      head | deletedvclock |
                      {n_val, pos_integer()} |
                      {sloppy_quorum, boolean()}.


%% Valid request options for get requests. When `if_modified' is
%% specified with a vclock, the request will fail if the object has
%% not changed. When `head' is specified, only the metadata will be
%% returned. When `deletedvclock' is specified, the vector clock of
%% the tombstone will be returned if the object has been recently
%% deleted.
-type put_option() :: write_quorum() | return_body | return_head | if_not_modified | if_none_match |
                      {n_val, pos_integer()} |
                      {sloppy_quorum, boolean()}.
%% Valid request options for put requests. `return_body' returns the
%% entire result of storing the object. `return_head' returns the
%% metadata from the result of storing the object. `if_not_modified'
%% will cause the request to fail if the local and remote vclocks do
%% not match. `if_none_match' will cause the request to fail if the
%% object already exists in Riak.
-type get_options() :: [get_option()]. %% A list of options for a get request.
-type put_options() :: [put_option()]. %% A list of options for a put request.
-type search_options() :: [search_option()]. %% A list of options for a search request.
-type delete_options() :: [delete_option()]. %% A list of options for a delete request.
-type mapred_queryterm() ::  {map, mapred_funterm(), Arg::term(), Accumulate :: boolean()} |
                             {reduce, mapred_funterm(), Arg::term(),Accumulate :: boolean()} |
                             {link, Bucket :: riakc_obj:bucket(), Tag :: term(), Accumulate :: boolean()}.
%% A MapReduce phase specification. `map' functions operate on single
%% K/V objects. `reduce' functions operate across collections of
%% inputs from other phases. `link' is a special type of map phase
%% that matches links in the fetched objects. The `Arg' parameter will
%% be passed as the last argument to the phase function. The
%% `Accumulate' param determines whether results from this phase will
%% be returned to the client.
-type mapred_funterm() :: {modfun, Module :: atom(), Function :: atom()} |
                          {qfun, function()} |
                          {strfun, list() | binary()} |
                          {jsanon, binary() | {bucket(), key()}} |
                          {jsfun, binary()}.
%% A MapReduce phase function specification. `modfun' requires that
%% the compiled module be available on all Riak nodes. `qfun' will
%% only work from the shell (compiled fun() terms refer to compiled
%% code only). `strfun' contains the textual source of an Erlang
%% function but the functionality must be enabled on the Riak cluster.
%% `jsanon' either contains javascript code that will be evaluated
%% as an anonymous function, or a bucket-value pair, pointing
%% to a record stored in riak containing the source of an anonymous
%% javascript function. `jsfun' contains the name of a javascript
%% function, that when evaluated points to a built-in javascript function.
-type mapred_result() :: [term()].
%% The results of a MapReduce job.
-type mapred_inputs() :: [{bucket(), key()} | {{bucket(), key()}, term()}] |
                         {modfun, Module::atom(), Function::atom(), [term()]} |
                         bucket() |
                         {index, bucket(), Index::binary()|secondary_index_id(), key()|integer()} |
                         {index, bucket(), Index::binary()|secondary_index_id(), StartKey::key()|integer(), EndKey::key()|integer()}.
%% Inputs for a MapReduce job.
-type connection_failure() :: {Reason::term(), FailureCount::integer()}.
%% The reason for connection failure and how many times that type of
%% failure has occurred since startup.
-type timeout_name() :: ping_timeout | get_client_id_timeout |
                        set_client_id_timeout | get_server_info_timeout |
                        get_timeout | put_timeout | delete_timeout |
                        list_buckets_timeout | list_buckets_call_timeout |
                        list_keys_timeout | stream_list_keys_timeout |
                        stream_list_keys_call_timeout | get_bucket_timeout |
                        get_bucket_call_timeout | set_bucket_timeout |
                        set_bucket_call_timeout | mapred_timeout |
                        mapred_call_timeout | mapred_stream_timeout |
                        mapred_stream_call_timeout | mapred_bucket_timeout |
                        mapred_bucket_call_timeout | mapred_bucket_stream_call_timeout |
                        search_timeout | search_call_timeout |
                        timeout.

-type continuation() :: 'undefined' | binary().
-type secondary_index_id() :: {binary_index, string()} | {integer_index, string()}.
-type index_term() :: integer() | binary().
-type keys() :: [binary()] | 'undefined'.
-type index_terms() :: [{index_term(), binary()}] | 'undefined'.

-record(index_results_v1, {
        keys :: keys(),
        terms :: index_terms(),
        continuation :: continuation()
        }).
-define(INDEX_RESULTS, #index_results_v1).
-type index_results() :: #index_results_v1{}.

-record(index_stream_result_v1, {
        keys :: keys(),
        terms :: index_terms()
        }).
-define(INDEX_STREAM_RESULT, #index_stream_result_v1).
-type index_stream_result() :: ?INDEX_STREAM_RESULT{}.

-type index_done() :: {'done', continuation()}.


-type search_option() ::
        {rows, non_neg_integer()} |  %% Limit rows
        {start, non_neg_integer()} | %% Starting offset
        {sort, binary()} |           %% sort order
        {filter, binary()} |         %% Inline fields filtering query
        {df, binary() } |            %% Default field
        {op, binary() } |            %% Default op
        {fl, [binary()]} |           %% Return fields limit (for ids only, generally)
        {presort, binary()}.         %% Presor (key / score)

-type search_doc() :: [{binary(), binary()}].
-type search_maxscore() :: float().
-type search_number_found() :: non_neg_integer().

-record(search_results, {
          docs :: [search_doc()],         %% Result documents
          max_score :: float(),           %% Maximum score
          num_found :: non_neg_integer()  %% Number of results
         }).

-type search_result() :: #search_results{}.
