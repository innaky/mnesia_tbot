%%%-------------------------------------------------------------------
%% @doc mnesia_tbot public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_tbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia_tbot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
