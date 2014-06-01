-ifndef(_cassandra_types_included).
-define(_cassandra_types_included, yeah).

-define(cassandra_ConsistencyLevel_ONE, 1).
-define(cassandra_ConsistencyLevel_QUORUM, 2).
-define(cassandra_ConsistencyLevel_LOCAL_QUORUM, 3).
-define(cassandra_ConsistencyLevel_EACH_QUORUM, 4).
-define(cassandra_ConsistencyLevel_ALL, 5).
-define(cassandra_ConsistencyLevel_ANY, 6).
-define(cassandra_ConsistencyLevel_TWO, 7).
-define(cassandra_ConsistencyLevel_THREE, 8).

-define(cassandra_IndexOperator_EQ, 0).
-define(cassandra_IndexOperator_GTE, 1).
-define(cassandra_IndexOperator_GT, 2).
-define(cassandra_IndexOperator_LTE, 3).
-define(cassandra_IndexOperator_LT, 4).

-define(cassandra_IndexType_KEYS, 0).
-define(cassandra_IndexType_CUSTOM, 1).
-define(cassandra_IndexType_COMPOSITES, 2).

-define(cassandra_Compression_GZIP, 1).
-define(cassandra_Compression_NONE, 2).

-define(cassandra_CqlResultType_ROWS, 1).
-define(cassandra_CqlResultType_VOID, 2).
-define(cassandra_CqlResultType_INT, 3).

%% struct column

-record(column, {name :: string() | binary(),
                 value :: string() | binary(),
                 timestamp :: integer(),
                 ttl :: integer()}).

%% struct superColumn

-record(superColumn, {name :: string() | binary(),
                      columns = [] :: list()}).

%% struct counterColumn

-record(counterColumn, {name :: string() | binary(),
                        value :: integer()}).

%% struct counterSuperColumn

-record(counterSuperColumn, {name :: string() | binary(),
                             columns = [] :: list()}).

%% struct columnOrSuperColumn

-record(columnOrSuperColumn, {column :: #column{},
                              super_column :: #superColumn{},
                              counter_column :: #counterColumn{},
                              counter_super_column :: #counterSuperColumn{}}).

%% struct notFoundException

-record(notFoundException, {}).

%% struct invalidRequestException

-record(invalidRequestException, {why :: string() | binary()}).

%% struct unavailableException

-record(unavailableException, {}).

%% struct timedOutException

-record(timedOutException, {acknowledged_by :: integer(),
                            acknowledged_by_batchlog :: boolean()}).

%% struct authenticationException

-record(authenticationException, {why :: string() | binary()}).

%% struct authorizationException

-record(authorizationException, {why :: string() | binary()}).

%% struct schemaDisagreementException

-record(schemaDisagreementException, {}).

%% struct columnParent

-record(columnParent, {column_family :: string() | binary(),
                       super_column :: string() | binary()}).

%% struct columnPath

-record(columnPath, {column_family :: string() | binary(),
                     super_column :: string() | binary(),
                     column :: string() | binary()}).

%% struct sliceRange

-record(sliceRange, {start :: string() | binary(),
                     finish :: string() | binary(),
                     reversed = false :: boolean(),
                     count = 100 :: integer()}).

%% struct slicePredicate

-record(slicePredicate, {column_names :: list(),
                         slice_range :: #sliceRange{}}).

%% struct indexExpression

-record(indexExpression, {column_name :: string() | binary(),
                          op :: integer(),
                          value :: string() | binary()}).

%% struct indexClause

-record(indexClause, {expressions = [] :: list(),
                      start_key :: string() | binary(),
                      count = 100 :: integer()}).

%% struct keyRange

-record(keyRange, {start_key :: string() | binary(),
                   end_key :: string() | binary(),
                   start_token :: string() | binary(),
                   end_token :: string() | binary(),
                   row_filter :: list(),
                   count = 100 :: integer()}).

%% struct keySlice

-record(keySlice, {key :: string() | binary(),
                   columns = [] :: list()}).

%% struct keyCount

-record(keyCount, {key :: string() | binary(),
                   count :: integer()}).

%% struct deletion

-record(deletion, {timestamp :: integer(),
                   super_column :: string() | binary(),
                   predicate :: #slicePredicate{}}).

%% struct mutation

-record(mutation, {column_or_supercolumn :: #columnOrSuperColumn{},
                   deletion :: #deletion{}}).

%% struct endpointDetails

-record(endpointDetails, {host :: string() | binary(),
                          datacenter :: string() | binary(),
                          rack :: string() | binary()}).

%% struct tokenRange

-record(tokenRange, {start_token :: string() | binary(),
                     end_token :: string() | binary(),
                     endpoints = [] :: list(),
                     rpc_endpoints :: list(),
                     endpoint_details :: list()}).

%% struct authenticationRequest

-record(authenticationRequest, {credentials = dict:new() :: dict()}).

%% struct columnDef

-record(columnDef, {name :: string() | binary(),
                    validation_class :: string() | binary(),
                    index_type :: integer(),
                    index_name :: string() | binary(),
                    index_options :: dict()}).

%% struct cfDef

-record(cfDef, {keyspace :: string() | binary(),
                name :: string() | binary(),
                column_type = "Standard" :: string() | binary(),
                comparator_type = "BytesType" :: string() | binary(),
                subcomparator_type :: string() | binary(),
                comment :: string() | binary(),
                read_repair_chance :: float(),
                column_metadata :: list(),
                gc_grace_seconds :: integer(),
                default_validation_class :: string() | binary(),
                id :: integer(),
                min_compaction_threshold :: integer(),
                max_compaction_threshold :: integer(),
                replicate_on_write :: boolean(),
                key_validation_class :: string() | binary(),
                key_alias :: string() | binary(),
                compaction_strategy :: string() | binary(),
                compaction_strategy_options :: dict(),
                compression_options :: dict(),
                bloom_filter_fp_chance :: float(),
                caching = "keys_only" :: string() | binary(),
                dclocal_read_repair_chance = 0.0 :: float(),
                populate_io_cache_on_flush :: boolean(),
                row_cache_size :: float(),
                key_cache_size :: float(),
                row_cache_save_period_in_seconds :: integer(),
                key_cache_save_period_in_seconds :: integer(),
                memtable_flush_after_mins :: integer(),
                memtable_throughput_in_mb :: integer(),
                memtable_operations_in_millions :: float(),
                merge_shards_chance :: float(),
                row_cache_provider :: string() | binary(),
                row_cache_keys_to_save :: integer()}).

%% struct ksDef

-record(ksDef, {name :: string() | binary(),
                strategy_class :: string() | binary(),
                strategy_options :: dict(),
                replication_factor :: integer(),
                cf_defs = [] :: list(),
                durable_writes = true :: boolean()}).

%% struct cqlRow

-record(cqlRow, {key :: string() | binary(),
                 columns = [] :: list()}).

%% struct cqlMetadata

-record(cqlMetadata, {name_types = dict:new() :: dict(),
                      value_types = dict:new() :: dict(),
                      default_name_type :: string() | binary(),
                      default_value_type :: string() | binary()}).

%% struct cqlResult

-record(cqlResult, {type :: integer(),
                    rows :: list(),
                    num :: integer(),
                    schema :: #cqlMetadata{}}).

%% struct cqlPreparedResult

-record(cqlPreparedResult, {itemId :: integer(),
                            count :: integer(),
                            variable_types :: list(),
                            variable_names :: list()}).

%% struct cfSplit

-record(cfSplit, {start_token :: string() | binary(),
                  end_token :: string() | binary(),
                  row_count :: integer()}).

-endif.
