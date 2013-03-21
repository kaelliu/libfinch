%%% File : mod_kernel.erl
%%% Description : db connection init,ets table init
%%% Author : kael liu
%%% Date : 2013-03-19 Tuesday
-module(mod_kernel).

%% ====================================================================
%% API functions
%% ====================================================================
-behaviour(gen_server).
-export([
            start_link/0,
            online_state/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
%% -include("record.hrl").

online_state()->
	case ets:info(?ETS_ONLINE,size) of
		undefined ->
            [0,0];
        Num when Num < 200 -> %
            [1, Num];
        Num when Num > 200 , Num < 500 -> %
            [2, Num];
        Num when Num > 500 , Num < 800 -> %
            [3, Num];
        Num when Num > 800 -> %
            [4, Num]  % more to come...
    end.

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init()->
	% ets
	ok = init_ets(),
    %% mysql
    ok = init_mysql(60),% temp setting
	{ok, 1}.

handle_cast(_R , Status) ->
    {noreply, Status}.

handle_call(_R , _FROM, Status) ->
    {reply, ok, Status}.

handle_info(_Reason, Status) ->
    {noreply, Status}.

terminate(normal, Status) ->
    {ok, Status}.

code_change(_OldVsn, Status, _Extra)->
    {ok, Status}.

init_mysql(ConnectionPoolSize) ->
%%     mysql:start_link(?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, fun(_, _, _, _) -> ok end, ?DB_ENCODE),
%%     mysql:connect(?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, ?DB_ENCODE, true),
	crypto:start(),
	emysql:start(),
	emysql:add_pool(?DB_POOLID, ConnectionPoolSize, ?DB_USER, ?DB_PASS, ?DB_HOST, ?DB_PORT, ?DB_NAME, ?DB_ENCODE),
	ok.

init_ets() ->
	% ets is cache system
    ets:new(?ETS_ONLINE, [{keypos,#ets_online.id}, named_table, public, set]), %% online user
	ok.
%% ====================================================================
%% Internal functions
%% ====================================================================


