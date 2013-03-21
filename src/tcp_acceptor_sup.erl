%%% File : tcp_acceptor_sup.erl
%%% Description : acceptor's supervisor
%%% Author : kael liu
%%% Date : 2012-08-01 Wednesday

-module(tcp_acceptor_sup).
-behaviour(supervisor).
-export([start_link/2]).
-export([init/1]).

start_link(Name,Callback)->
	supervisior:start_link({local,Name},?MODULE,Callback).

init(Callback)->
	{ok,
		{
			{simple_one_for_one,10,10},
			[{tcp_acceptor,{tcp_acceptor,start_link,[Callback]},
			transient,brutal_kill,worker,[tcp_acceptor]}]
		}
	}.
