%%%-------------------------------------------------------------------
%% @doc mnesia_tbot public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_tbot_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/telegram_users.hrl").

start(_StartType, _StartArgs) ->
    mnesia_tbot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
