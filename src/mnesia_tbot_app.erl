%%%-------------------------------------------------------------------
%% @doc mnesia_tbot public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_tbot_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([insert_user/1, install/1, get_user/1, get_users/0]).
-export([op/2, deop/1, get_admin/1, get_admins/0, is_admin/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("include/telegram_users.hrl").

-define(OPHASH, <<206,163,197,225,210,222,44,172,93,222,208,63,120,146,159,78,207,203,233,237,49,211,196,182,194,112,102,35>>).

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

get_user(Username) ->
    F = fun() ->
		Q = qlc:q([{U#telegram_users.username, U#telegram_users.id,
			    U#telegram_users.localtime} 
			   || U <- mnesia:table(telegram_users),
			      U#telegram_users.username == Username]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).

get_users() ->
    F = fun() ->
		Q = qlc:q([{U#telegram_users.username, U#telegram_users.id,
			    U#telegram_users.localtime}
			   || U <- mnesia:table(telegram_users)]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).

op(Password, Username) ->
    case is_hash_valid(Password) of
	true ->
	    {_, User} = get_user(Username),
	    case User of
		[] ->
		    false;
		_ ->
		    [{Name, Id, _}] = User,
		    Localtime = format_time(),
		    NewOp = #telegram_admins{id = Id, username = Name, localtime = Localtime},
		    T = fun() ->
				mnesia:write(NewOp)
			end,
		    mnesia:transaction(T),
		    ok
	    end;
	_ ->
	    false
    end.

deop(Username) ->
    {_, User} = get_admin(Username),
    case User of
	[] ->
	    false;
	_ ->
	    [{_, Id, Name, Localtime}] = User,
	    AdminDEL = #telegram_admins{id = Id, username = Name, localtime = Localtime},
	    T = fun() ->
			mnesia:delete_object(telegram_admins, AdminDEL, write)
		end,
	    mnesia:transaction(T),
	    ok
    end.

get_admin(Username) ->
    T = fun() ->
		Q = qlc:q([A || A <- mnesia:table(telegram_admins),
				A#telegram_admins.username == Username]),
		qlc:e(Q)
	end,
    mnesia:transaction(T).

get_admins() ->
    T = fun() ->
		Q = qlc:q([{A#telegram_admins.id, A#telegram_admins.username,
			    A#telegram_admins.localtime}
			   || A <- mnesia:table(telegram_admins)]),
		qlc:e(Q)
	end,
    mnesia:transaction(T).

is_admin(ID) ->
    {_, Id} = get_admin_by_id(ID),
    case Id of
	[] ->
	    false;
	_ ->
	    true
    end.
		
%% internal functions
format_time() ->
    {{Y, M, D}, {H, Min, S}} = erlang:localtime(),
    erlang:list_to_binary(io_lib:format("~p:~p:~p ~p/~p/~p", [H, Min, S, D, M, Y])).

is_hash_valid(Password) ->
    crypto:hash(sha224, Password) == ?OPHASH.

get_admin_by_id(ID) ->
    T = fun() ->
		Q = qlc:q([A#telegram_admins.id || A <- mnesia:table(telegram_admins),
						   A#telegram_admins.id == ID]),
		qlc:e(Q)
	end,
    mnesia:transaction(T).
