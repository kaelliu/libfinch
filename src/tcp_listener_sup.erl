%%% File : tcp_listener_sup.erl
%%% Description : tcp listener's supervisor
%%% Author : kael liu
%%% Date : 2012-07-31 Tuesday

-module(tcp_listener_sup).
-behaviour(supervisor).
%% external function
-export([start_link/7,start_link/8]).
%% callback
-export([init/1]).

start_link(IPAddress,Port,SocketOpts,OnStartup,OnShutdown,AcceptCallback,Label)->
	 start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
             AcceptCallback, 1, Label).

start_link(IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
           AcceptCallback, ConcurrentAcceptorCount, Label) ->
  	supervisor:start_link(?MODULE,
    	{IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
    	AcceptCallback, ConcurrentAcceptorCount, Label}).	

init({IPAddress, Port, SocketOpts, OnStartup, OnShutdown,
		AcceptCallback, ConcurrentAcceptorCount, Label})->
	Name = finch_netutil:tcp_name(tcp_acceptor_sup,IPAddress,Port),
	{ok,{
			{one_for_all,10,10},
			[
				{
					tcp_acceptor_sup,{tcp_acceptor_sup,
						start_link,[Name,AcceptCallback]},
						transient, %% transient child process should be restarted only if it terminates abnormally
						infinity,supervisor,[tcp_acceptor_sup]
				},
			 	{
					tcp_listener,{tcp_listener,start_link,[IPAddress,Port,SocketOpts,
								   ConcurrentAcceptorCount, Name,
                           		   OnStartup, OnShutdown, Label]},
						   transient,16#ffffffff,worker,[tcp_listener]
			 	}
		 	]
		}
	}.
