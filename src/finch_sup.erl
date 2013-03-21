%%% File : finch_sup.erl
%%% Description : server's supervisor,control other process's startup and stop
%%% Author : kael liu
%%% Date : 2012-07-31 Tuesday

-module(finch_sup).
-include("common.hrl").
-behaviour(supervisor).
%%% functions
-export([start_link/0,start_child/1,start_child/2,start_child/3,
		start_supervisor_child/1,start_supervisor_child/2,
		start_supervisor_child/3,stop_child/1]).
%%% behaviour callbacks
-export([init/1]).

%%% specs
-ifdef(use_specs).
-spec(start_link/0 :: () -> pid).
-spec(start_child/1 :: (atom()) -> 'ok').
-spec(start_child/2 :: (atom(), [any()]) -> 'ok').
-spec(start_child/3 :: (atom(), atom(), [any()]) -> 'ok').
-spec(start_supervisor_child/1 :: (atom()) -> 'ok').
-spec(start_supervisor_child/2 :: (atom(), [any()]) -> 'ok').
-spec(start_supervisor_child/3 :: (atom(), atom(), [any()]) -> 'ok').
-spec(stop_child/1 :: (atom()) -> ok | error).
-endif.

start_link() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

start_child(Mod)->
	start_child(Mod,[]).

start_child(Mod,Args)->
	start_child(Mod,Mod,Args).

start_child(ChildId,Mod,Args)->
	child_reply(supervisor:start_child(?MODULE,
			{ChildId,{Mod,start_link,Args},
			transient,?MAX_WAIT,worker,[Mod]})).

start_supervisor_child(Mod)-> 
	start_supervisor_child(Mod,[]).

start_supervisor_child(Mod,Args)->
	start_supervisor_child(Mod,Mod,Args).

start_supervisor_child(ChildId,Mod,Args)->
	child_reply(supervisor:start_child(?MODULE,
			{ChildId,{Mod,start_link,Args},
			transient,infinity,supervisor,[Mod]})).

stop_child(ChildId)->
	case supervisor:terminate_child(?MODULE,ChildId) of
		ok -> supervisor:delete_child(?MODULE,ChildId);
		E  -> E
	end.
% when stop,not restart-0,1
init([])->{ok,{{one_for_all,0,1},[]}}.

%%--------------------------------------------------------------------
child_reply({ok,_}) ->ok;
child_reply(X)		->X.
