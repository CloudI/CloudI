ErlasticSearch
=========
A thrift based erlang client for [ElasticSearch](http://www.elasticsearch.org/).

It incorporates a connection-pool based on [poolboy](https://github.com/devinus/poolboy) - if/when you get down to productizing, you might want to take a look at the pool's `size` and `max_overflow` options



Installation
============
Add this as a rebar dependency to your project.

1. Be sure to set up ElasticSearch to support thrift!
   * Install the thrift plugin (available [here](https://github.com/elasticsearch/elasticsearch-transport-thrift))
      * Probably something like --> ```bin/plugin -install elasticsearch/elasticsearch-transport-thrift/1.5.0.```
   * You'll need to add (at least) the following settings to config.yaml
      * ```thrift.port: 9500```
      * ```thrift.protocol: 'binary'```
    * You might want to set the port to whatever you want instead of ```9500```.  Mind you, in that case you might need to update ```app.config``` and/or your ```connection_options``` in your application/erlasticsearch setup too.
    * Start ElasticSearch
       * If you plan on running the tests, you probably want to do this.
       * Heck, if you plan on using ElasticSearch, you probably want to do this.
       * If you plan on running the tests, you might want to run it in 'in-memory' mode.
          * Probably something like --> ```elasticsearch -f -Des.index.storage.type=memory -Des.config=/usr/local/opt/elasticsearch/config/elasticsearch.yml```
1. Update your environment with the following ```connection_options``` (look in [app.config](https://github.com/dieswaytoofast/erlasticsearch/blob/master/app.config) for examples)
   * ```thrift_options``` (default : *[{framed, true}]*)
   * ```thrift_host``` (default : *"localhost"*)
   * ```thrift_port``` (default : *9500*)
   * ```binary_response``` (default *true*. When *false*, this will run ```jsx:decode``` on the ES response, and send the tuples back to you, instead of one long binary)
   * ```pools```
      * If you are using the default pools, be sure to use the (uncommented) pool settings from ```app.config``` ( ***If you use the default pools, then you will have to start up elasticsearch before the application, otherwise Bad Things™ will happen*** )
1. Start a pool
	* ```erlasticsearch:start_pool(<<"some_unique_name_here">>).```
1. Profit


WARNING
============
__**THE TESTS WILL CREATE AND DELETE INDICES IN WHATEVER ELASTICSEARCH INSTANCE YOU POINT THE CLIENT AT**__

__**THE TESTS WILL CREATE AND DELETE INDICES IN WHATEVER ELASTICSEARCH INSTANCE YOU POINT THE CLIENT AT**__

__**THE TESTS WILL CREATE AND DELETE INDICES IN WHATEVER ELASTICSEARCH INSTANCE YOU POINT THE CLIENT AT**__

__**!!!!!!!SERIOUSLY!!!!!!**__


__**YOU HAVE BEEN WARNED**__


TL;DR
============

1. Make sure you have ElasticSearch running.
1. You can explicitly start a (new) connection pool, and access ElasticSearch that way, as follows (e.g. if you need distinct pools for distinct Indexes)
   * Start up a pool ---> ```erlasticsearch:start_pool(<<"some_unique_name_here">>).```
   * From that point, use ```<<"some_unique_name_here">>``` as ```ServerRef```, e.g. 
```erlang
(erlasticsearch@paglierino)1 > erlasticsearch:is_index(<<"some_unique_name_here">>, <<"an_index">>).  
```
1. You can just pick ```<<"another_unique_name">>>``` and use it as ```ServerRef```, *without* starting up a connection pool. In this case, a connection pool will be automagically started. e.g.
```erlang
(erlasticsearch@paglierino)1 > erlasticsearch:is_index(<<"some_unique_name_here">>, <<"an_index">>).  
```
1. Any JSON expected by ElasticSearch will need to go in as JSON or [jsx](https://github.com/talentdeficit/jsx) encodable proplists. e.g.
   * ```<<"{\"settings\":{\"number_of_shards\":3}}">>```, or
   * ```[{<<"settings">>, [{<<"number_of_shards">>, 3}]}]```
1. Output returned by everything is in the form of ```[tuple()] | {error, Reason}```, i.e., either it is a list of tuples, or an error.  The tuple list will contain the following
   * **{status, Status}** <-- This is the REST code (200, 201, 404, etc.) 
   * **{body, Body}** <-- The body of the response from ElasticSearch. More on this next
   * **{result, Result}** <-- A boolean representing the result for the various boolean methods (```is_index```, ```is_doc```, etc.)
   * The Body of the response _from_ ElasticSearch - when it exists - will be JSON.  That said, ```binary_response``` in your ```connection_options``` is going to determine the form of the response.  
      * The default is ```binary_response = true```. In this case, you ```{body, Body}``` is just going to contain the entire payload from Elasticsearch as a single binary.
         * e.g. --> ```{body , <<"{\"ok\":true,\"acknowledged\":true}">>}```
      * If you set ```binary_response = false```, ```{body, Body}``` will contain the JSON as a decoded tuple-list (basically, what you get by running ```jsx:decode(Body)```)
         * ```{body , [ {<<"ok">> , true} , {<<"acknowledged">> , true} ] }```




Details
============
*[Supervisor tree diagram][sup_diagram]:*

[sup_diagram]: https://github.com/dieswaytoofast/erlasticsearch/blob/master/supervision_tree.jpg


Pool Management
-----
These methods are available to start and stop the thrift pools 
Once the erlasticsearch application has been started, you can use ```start_pool``` and ```stop_pool``` to start/stop a connection-pool.  After this, you can use your _PoolName_ as ```ServerRef``` in the accessors below.   
Alternatively, you can just use _PoolName_, and a pool will be started up for you. The difference here is that the defaults will get used for the pool's ```size``` and ```max_overflow``` options

_PoolName_ can also be used to refer to multiple ElasticSearch instances. This is because in all cases, _PoolName_ is one of

   * ```PoolIdentifier``` e.g.
     * ```<<"some unique name">>```)
   * ```{ThriftHost, ThriftPort, PoolIdentifier}``` e.g.
       * ```{"localhost", 9500, <<"some unqiue name">>}```
       * ```{unidentified, 9500, <<"some unique name">>}```

The type spec

```erlang
-type pool_identifier() :: binary().
-type thrift_host()     :: undefined | string().
-type thrift_port()     :: undefined | integer().
```


Function | Parameters | Description
----- | ----------- | --------
start_pool/1 | PoolName  | Start a connection pool referenceable as _PoolName_, with default ```pool_options``` and ```connection_options```
start_pool/2 | PoolName, PoolParameters | Start a connection pool referenceable as _PoolName_, with custom ```pool_options``` and default ```connection_options```
start_pool/3 | PoolName, PoolParameters, ConnectionParameters | Start a connection pool referenceable as _PoolName_, with custom ```pool_options``` and ```connection_options```
stop_pool/1 | PoolName  | Stop the connection pool referenced as _PoolName_

**EXAMPLES**

Using the _client_ based accessors  (note that ```bar2``` has ```{binary_response, false}```)


```erlang
erlasticsearch@pecorino)1> erlasticsearch:start_pool(<<"bar1">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)2> {ok, Pid} = erlasticsearch:start_pool(<<"bar2">>, [{thrift_options, [{framed, false}]}, {binary_response, false}]).
{ok,<0.182.0>}
erlasticsearch@pecorino)4> erlasticsearch:flush(<<"bar1">>).
[{status,<<"200">>},
 {body,<<"{\"ok\":true,\"_shards\":{\"total\":0,\"successful\":0,\"failed\":0}}">>}]
erlasticsearch@pecorino)5> erlasticsearch:flush(<<"barbar">>).
{ok,{restResponse,200,undefined,<<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}}
erlasticsearch@pecorino)8> erlasticsearch:stop_pool(<<"bar1">>).
ok
erlasticsearch@pecorino)9> erlasticsearch:stop_pool(<<"bar2">>).
ok
erlasticsearch@pecorino)9> erlasticsearch:stop_pool(<<"barbar">>).
ok
```

**Note that the pool associated with ```<<"barbar">>``` was automatically started up by erlasticsearch above!**


Index CRUD
-----
These methods are available to perform CRUD activities on Indexes (kinda, sorta, vaguely the equivalent of Databases in ElasticSearch. But you already knew that, right?)

Function | Parameters | Description
----- | ----------- | --------
create_index/2 | ServerRef, IndexName  | Creates the Index called _IndexName_
create_index/3 | ServerRef, IndexName, Parameters | Creates the Index called _IndexName_, with additional options as specified [here](http://www.elasticsearch.org/guide/reference/api/admin-indices-create-index/)
delete_index/2 | ServerRef, IndexName  | Deletes the Index called _IndexName_
is_index/2 | ServerRef, IndexName  | Checks if the Index called _IndexName_ exists. (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```)
is_type/3 | ServerRef, IndexName, TypeName  | Checks if the Type called _TypeName exists in the index _IndexName_. (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```), as well as a list of types (e.g. ```[<<"type1">>, <<"type2">>]```)
open_index/2 | ServerRef, IndexName  | Opens the Index called _IndexName_
close_index/2 | ServerRef, IndexName  | Closes the Index called _IndexName_



**EXAMPLES**

(note that ```bar2``` has ```{binary_response, false}```)

```erlang
erlasticsearch@pecorino)3> erlasticsearch:create_index(<<"bar">>, <<"foo2">>).
[{status,<<"200">>},
 {body,<<"{\"ok\":true,\"acknowledged\":true}">>}]
erlasticsearch@pecorino)6> erlasticsearch:delete_index(<<"bar">>, <<"foo2">>).                                               
[{status,<<"200">>},
 {body,<<"{\"ok\":true,\"acknowledged\":true}">>}]
erlasticsearch@pecorino)8> erlasticsearch:is_index(<<"an_erlasticsearch_pool">>, <<"foo3">>).    
[{result,<<"false">>},{status,<<"404">>}]
```



Document CRUD
-----
These methods are available to perform CRUD activities on actual documents 

(**Note**: _ServerRef_ is either a _Client Name/Reference_, or _{pool, PoolName}_)

Function | Parameters | Description
----- | ----------- | --------
insert_doc/5 | ServerRef, IndexName, Type, Id, Doc  | Creates the Doc under _IndexName_, with type _Type_, and id _Id_
insert_doc/6 | ServerRef, IndexName, Type, Id, Doc, Params  | Creates the Doc under _IndexName_, with type _Type_, and id _Id_, and passes the tuple-list _Params_ to ElasticSearch
is_doc/4 | ServerRef, IndexName, Type, Id  | Checks if the Doc under _IndexName_, with type _Type_, and id _Id_ exists
get_doc/4 | ServerRef, IndexName, Type, Id  | Gets the Doc under _IndexName_, with type _Type_, and id _Id_
get_doc/5 | ServerRef, IndexName, Type, Id, Params  | Gets the Doc under _IndexName_, with type _Type_, and id _Id_, and passes the tuple-list _Params_ to ElasticSearch
mget_doc/2 | ServerRef, Doc  | Gets documents from the ElasticSearch cluster based on the Index(s), Type(s), and Id(s) in _Doc_
mget_doc/3 | ServerRef, IndexName, Doc  | Gets documents from the ElasticSearch cluster index _IndexName_ based on the Type(s), and Id(s) in _Doc_
mget_doc/4 | ServerRef, IndexName, TypeName, Doc  | Gets documents from the ElasticSearch cluster index _IndexName_, with type _TypeName_, based on the Id(s) in _Doc_
delete_doc/4 | ServerRef, IndexName, Type, Id  | Deleset the Doc under _IndexName_, with type _Type_, and id _Id_
delete_doc/5 | ServerRef, IndexName, Type, Id, Params  | Deletes the Doc under _IndexName_, with type _Type_, and id _Id_, and passes the tuple-list _Params_ to ElasticSearch
count/2 | ServerRef, Doc | Counts the docs in the cluster based on the search in _Doc_. (note that if _Doc_ is empty, you get a count of all the docs in the cluster)
count/3 | ServerRef, Doc, Params | Counts the docs in the cluster based on the search in _Doc_, using _Params_.  Note that either _Doc_ or _Params_ can be empty, but clearly not both :-)
count/4 | ServerRef, IndexName, Doc, Params | Counts the docs in the cluster based on the search in _Doc_, associated with the index _IndexName_, using _Params_  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```. This list can also be empty - ```[]```)
count/5 | ServerRef, IndexName, TypeName, Doc, Params | Counts the docs in the cluster based on the search in _Doc_, associated with the index _IndexName_, and type _TypeName_ using _Params_  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```, as well as a list of types (e.g. ) ```[<<"type1">>, <<"type2">>]```. Each of these lists can also be empty - ```[]```)
delete_by_query/2 | ServerRef, Doc | Deletes the docs in the cluster based on the search in _Doc_. (note that if _Doc_ is empty, you get a count of all the docs in the cluster)
delete_by_query/3 | ServerRef, Doc, Params | Deletes the docs in the cluster based on the search in _Doc_, using _Params_.  Note that either _Doc_ or _Params_ can be empty, but clearly not both :-)
delete_by_query/4 | ServerRef, IndexName, Doc, Params | Deletes the docs in the cluster based on the search in _Doc_, associated with the index _IndexName_, using _Params_  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```. This list can also be empty - ```[]```)
delete_by_query/5 | ServerRef, IndexName, TypeName, Doc, Params | Deletes the docs in the cluster based on the search in _Doc_, associated with the index _IndexName_, and type _TypeName_ using _Params_  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```, as well as a list of types (e.g. ) ```[<<"type1">>, <<"type2">>]```. Each of these lists can also be empty - ```[]```)




_Note_: 

1. For both ```insert_doc/4``` and ```insert_doc/5```, sending in ```undefined``` as the ```Id``` will result in ElasticSearch generating an Id for the document.  This Id will be returned as part of the result...
2. Yes, the order of the arguments to mget_doc/[2,3,4] is weird.  Its just that ElasticSearch is slightly strange in this one...

**EXAMPLES**

```erlang
erlasticsearch@pecorino)4> erlasticsearch:start_pool(<<"bar">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)5> erlasticsearch:start_pool(<<"bar2">>, [{binary_response, false}]).
{ok,<0.182.0>}
erlasticsearch@pecorino)6> erlasticsearch:insert_doc(<<"bar">>, <<"index1">>, <<"type1">>, <<"id1">>, <<"{\"some_key\":\"some_val\"}">>).
[{status,<<"201">>},
 {body,<<"{\"ok\":true,\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":1}">>}]
erlasticsearch@pecorino)7> erlasticsearch:insert_doc(<<"bar">>, <<"index2">>, <<"type3">>, <<"id2">>, <<"{\"some_key\":\"some_val\"}">>, [{'_ttl', '1d'}]). 
[{status,<<"201">>},
 {body,<<"{\"ok\":true,\"_index\":\"index2\",\"_type\":\"type3\",\"_id\":\"id2\",\"_version\":1}">>}]
erlasticsearch@pecorino)8> erlasticsearch:insert_doc(<<"bar2">>, <<"index3">>, <<"type3">>, undefined, <<"{\"some_key\":\"some_val\"}">>).
[{status,201},
 {body,[{<<"ok">>,true},
        {<<"_index">>,<<"index3">>},
        {<<"_type">>,<<"type3">>},
        {<<"_id">>,<<"z9M78se6SuKsZ0lYlybAwg">>},
        {<<"_version">>,1}]}]
erlasticsearch@pecorino)10> erlasticsearch:get_doc(<<"bar2">>, <<"index1">>, <<"type1">>, <<"id1">>, [{fields, foobar}]).
[{status,200},
 {body,[{<<"_index">>,<<"index1">>},
        {<<"_type">>,<<"type1">>},
        {<<"_id">>,<<"id1">>},
        {<<"_version">>,1},
        {<<"exists">>,true}]}]
erlasticsearch@pecorino)11> erlasticsearch:get_doc(<<"bar2">>, <<"index1">>, <<"type1">>, <<"id1">>, [{fields, some_key}]).
[{status,200},
 {body,[{<<"_index">>,<<"index1">>},
        {<<"_type">>,<<"type1">>},
        {<<"_id">>,<<"id1">>},
        {<<"_version">>,1},
        {<<"exists">>,true},
        {<<"fields">>,[{<<"some_key">>,<<"some_val">>}]}]}]
```


Search
-----
API to perform searches against ElasticSearch (this _is_ why you are using ElasticSearch, right?)

Function | Parameters | Description
----- | ----------- | --------
search/4 | PoolName, IndexName, Type, Doc  | Searches the index _IndexName_, with type _Type_ for the JSON query embedded in _Doc_
search/5 | PoolName, IndexName, Type, Doc, Params  | Searches the index _IndexName_, with type _Type_ for the JSON query embedded in _Doc_, and passes the tuple-list _Params_ to ElasticSearch



**EXAMPLES**

```erlang
erlasticsearch@pecorino)2> erlasticsearch:insert_doc({"localhost", 9500, <<"an_erlasticsearch_pool">>}, <<"index1">>, <<"type1">>, <<"id1">>, <<"{\"some_key\":\"some_val\"}">>).
[{status,<<"201">>},
 {body, <<"{\"ok\":true,\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":1}">>}]
erlasticsearch@pecorino)3> erlasticsearch:search({"localhost", 9500, <<"an_erlasticsearch_pool">>}, <<"index1">>, <<"type1">>, <<>>, [{q, "some_key:some_val"}]).     
[{status,<<"200">>},
 {body, <<"{\"took\":1,\"timed_out\":false,\"_shards\":{\"total\":5,\"successful\":5,\"failed\":0},\"hits\":{\"total\":"...>>}]
```

Index Helpers
-----
A bunch of functions that do "things" to indices (flush, refresh, etc.)

(**Note**: _ServerRef_ refers to either _PoolName_ or _{ThriftHost, ThriftPort, PoolIdentifier}_)

Function | Parameters | Description
----- | ----------- | --------
flush/1 | ServerRef  | Flushes all the indices
flush/2 | ServerRef, Index | Flushes the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```)
optimize/1 | ServerRef  | Optimizes all the indices
optimize/2 | ServerRef, Index | Optimizes the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
segments/1 | ServerRef  | Provides segment information for all the indices in the cluster
segments/2 | ServerRef, Index | Provides segment information for the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
refresh/1 | ServerRef  | Refreshes all the indices
refresh/2 | ServerRef, Index | Refreshes the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
status/2 | ServerRef, Index | Returns the status of index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
clear_cache/1 | ServerRef  | Clears all the caches in the cluster
clear_cache/2 | ServerRef, Index | Clears all the caches associated with the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
clear_cache/3 | ServerRef, Index, params | Clears all the caches associated with the index _IndexName_, using _Params_  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)


**EXAMPLES**

```erlang
erlasticsearch@pecorino)2> erlasticsearch:refresh(<<"bar">>).                                                                                   [{status,<<"200">>},
 {body, <<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}] 
erlasticsearch@pecorino)3> erlasticsearch:refresh(<<"bar">>, <<"index1">>).                                                                        [{status,<<"200">>},
 {body, <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}">>}]
erlasticsearch@pecorino)4> erlasticsearch:refresh(<<"bar">>, [<<"index1">>, <<"index2">>]).
[{status,<<"200">>},
 {body, <<"{\"ok\":true,\"_shards\":{\"total\":16,\"successful\":8,\"failed\":0}}">>}]
erlasticsearch@pecorino)5> erlasticsearch:flush(<<"bar">>).                                                                                   [{status,<<"200">>},
 {body, <<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}] 
erlasticsearch@pecorino)6> erlasticsearch:refresh(<<"bar">>, <<"index1">>).                                                                        [{status,<<"200">>},
 {body, <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}">>}]
erlasticsearch@pecorino)7> erlasticsearch:refresh(<<"bar">>, [<<"index1">>, <<"index2">>]).
[{status,<<"200">>},
 {body, <<"{\"ok\":true,\"_shards\":{\"total\":16,\"successful\":8,\"failed\":0}}">>}] 
```


Cluster Helpers
-----
A bunch of functions that do "things" to clusters (health, etc.)

(**Note**: _ServerRef_ refers to either _PoolName_ or _{ThriftHost, ThriftPort, PoolIdentifier}_)

Function | Parameters | Description
----- | ----------- | --------
health/1 | ServerRef  | Reports the health of the cluster
state/1 | ServerRef  | Reports the state of the cluster
state/2 | ServerRef, Params  | Reports the state of the cluster, with optional parameters
nodes_info/1 | ServerRef  | Reports the state of all the nodes in the cluster
nodes_info/2 | ServerRef, NodeName  | Reports the state of the node _NodeName_ in the cluster. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]``` This list can also be empty - ```[]```)
nodes_info/3 | ServerRef, NodeName, Params  | Reports the state of the node _NodeName_ in the cluster, with optional _Params_. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]``` This list can also be empty - ```[]```)
nodes_stats/1 | ServerRef  | Reports stats on all the nodes in the cluster
nodes_stats/2 | ServerRef, NodeName  | Reports the stats of the node _NodeName_ in the cluster. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]```)
nodes_stats/3 | ServerRef, NodeName, Params  | Reports the stats of the node _NodeName_ in the cluster, with optional _Params_. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]``` This list can also be empty - ```[]```)


**EXAMPLES**
```erlang
erlasticsearch@pecorino)2> erlasticsearch:refresh(<<"bar">>).                                                                                   {ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}]
erlasticsearch@pecorino)3> erlasticsearch:health(<<"bar">>).                          
[{status,<<"200">>},
 {body, <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"status\":\"yellow\",\"timed_out\":false,\"number_of_nodes\""...>>}]
erlasticsearch@pecorino)1> erlasticsearch:stop_client(<<"bar">>).
ok
erlasticsearch@pecorino)4> erlasticsearch:state(<<"bar">>).
[{status,<<"200">>},
 {body, <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"master_node\":\"7k3ViuT5SQ67ayWsF1y8hQ\",\"blocks\":{\"ind"...>>}]
erlasticsearch@pecorino)5> erlasticsearch:state(<<"bar">>, [{filter_nodes, true}]).
[{status,<<"200">>},
 {body, <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"blocks\":{\"indices\":{\"index1\":{\"4\":{\"description\":\"inde"...>>}]
```


Credits
=======

Thanks to [Paul Oliver](https://github.com/puzza007) for helping with the [poolboy](https://github.com/devinus/poolboy) implementation

This is _not_ to be confused with [erlastic_search](https://github.com/tsloughter/erlastic_search) by [Tristan Sloughter](https://github.com/tsloughter), which is HTTP/REST based, and almost certainly did not involve quite this level of head-thumping associated w/ figuring out how Thrift works…

(Yes, this is a _Credit_)
