-type precision() :: n | u | ms | s | m | h.
-type protocol() :: http | udp.

-record(state, {protocol :: protocol(),
                db :: binary(), % for http
                username :: undefined | binary(), % for http
                password :: undefined | binary(), % for http
                host :: inet:ip_address() | inet:hostname(), % for udp
                port :: inet:port_number(),  % for udp
                timestamping :: boolean(),
                precision :: precision(),
                collected_metrics = <<>> :: binary(),
                batch_window_size :: non_neg_integer(),
                max_udp_size :: pos_integer(),
                tags :: map(),
                series_name :: atom() | binary(),
                formatting :: list(),
                metrics :: map(),
                autosubscribe :: boolean(),
                subscriptions_module :: module(),
                connection :: gen_udp:socket() | reference()}).

-type state() :: #state{}.
