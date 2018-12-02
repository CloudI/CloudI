

# Module elli_middleware #
* [Description](#description)

HTTP request processing middleware.

__Behaviours:__ [`elli_handler`](elli_handler.md).

<a name="description"></a>

## Description ##

This module offers both pre-processing of requests and post-processing of
responses. It can also be used to allow multiple handlers, where the first
handler to return a response short-circuits the request.
It is implemented as a plain elli handler.

Usage:

```
   Config = [
             {mods, [
                     {elli_example_middleware, []},
                     {elli_middleware_compress, []},
                     {elli_example_callback, []}
                    ]}
            ],
   elli:start_link([
                    %% ...,
                    {callback, elli_middleware},
                    {callback_args, Config}
                   ]).
```

The configured modules may implement the elli behaviour, in which case all
the callbacks will be used as normal. If [`handle/2`](#handle-2) returns `ignore`,
elli will continue on to the next callback in the list.

Pre-processing and post-processing is implemented in [`preprocess/2`](#preprocess-2)
and [`postprocess/3`](#postprocess-3). [`preprocess/2`](#preprocess-2) is called for each
middleware in the order specified, while [`postprocess/3`](#postprocess-3) is called in
the reverse order.
