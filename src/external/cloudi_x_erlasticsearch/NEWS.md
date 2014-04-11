NEWS
=========

1.4.6
----

* The version numbers in the tag have changed. There is no __'v'__ as a prefix,
i.e. it is _1.4.6_, not __v__*1.4.6*

1.4
---

* Pools are referenced directly as ```<<"some_name">>```, instead of ```{pool, <<"some_name">>}```.  e.g.
```erlang
(erlasticsearch@paglierino)1 > erlasticsearch:start_pool(<<"some_unique_name_here">>).
```
* Removed the 'standalone gen_server' way of doing things. Its pools right all the way down.
* Don't need to actually start a pool - one will be started for you if necessary. e.g.
```erlang
erlasticsearch@pecorino)1> erlasticsearch:flush(<<"bar1">>).
[{status,<<"200">>},
 {body,<<"{\"ok\":true,\"_shards\":{\"total\":0,\"successful\":0,\"failed\":0}}">>}]
 ```
