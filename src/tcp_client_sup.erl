%%% File : tcp_client_sup.erl
%%% Description : client sup 
%%% Author : kael liu
%%% Date : 2012-08-01 Wednesday
-module(tcp_client_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Callback)->
	supervisor:start_link(?MODULE,Callback).
start_link(SupName,Callback)->
	supervisor:start_link(SupName,?MODULE,Callback).

init({M,F,A})->
	{ok,{{simple_one_for_one,0,1},
		[{
			M,{M,F,A},temporary,brutal_kill,worker,[M]
		}]}
	}.
