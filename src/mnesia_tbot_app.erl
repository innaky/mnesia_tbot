%%%-------------------------------------------------------------------
%% @doc mnesia_tbot public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_tbot_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([insert_user/1, install/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/telegram_users.hrl").

start(_StartType, _StartArgs) ->
    mnesia_tbot_sup:start_link().

stop(_State) ->
    ok.

install(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(telegram_users, [{attributes, record_info(fields, telegram_users)},
					 {disc_copies, Nodes}]),
    mnesia:create_table(telegram_admins, [{attributes, record_info(fields, telegram_admins)},
					  {disc_copies, Nodes}]),
    mnesia:create_table(phones, [{attributes, record_info(fields, phones)},
				 {disc_copies, Nodes}]).

insert_user(User) ->
    Id = maps:get(id, User),
    Username = maps:get(username, User),
    Localtime = format_time(),
    Mnuser = #telegram_users{id = Id, username = Username, localtime = Localtime},
    U = fun() ->
		mnesia:write(Mnuser)
	end,
    mnesia:transaction(U).
		
%% internal functions
format_time() ->
    {{Y, M, D}, {H, Min, S}} = erlang:localtime(),
    erlang:list_to_binary(io_lib:format("~p:~p:~p ~p/~p/~p", [H, Min, S, D, M, Y])).

