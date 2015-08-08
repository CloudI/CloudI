-module(cloudi_x_syslog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cloudi_x_syslog_sup:start_link().

stop(_State) ->
    ok.
