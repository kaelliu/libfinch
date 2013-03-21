%%% File : tcp_listener.erl
%%% Description : tcp listener's worker process
%%% Author : kael liu
%%% Date : 2012-07-31 Tuesday

-module(tcp_listener).
-behaviour(gen_server).
%% externel functions
-export([start_link/8]).
%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {sock, on_startup, on_shutdown, label}).

%%--------------------------------------------------------------------
%% start
%%--------------------------------------------------------------------
start_link(IPAddress, Port, SocketOpts,
           ConcurrentAcceptorCount, AcceptorSup,
           OnStartup, OnShutdown, Label) ->
    gen_server:start_link(
      ?MODULE, {IPAddress, Port, SocketOpts,
                ConcurrentAcceptorCount, AcceptorSup,
                OnStartup, OnShutdown, Label}, []).

%%--------------------------------------------------------------------
%% init callback
%%--------------------------------------------------------------------
init({IPAddress, Port, SocketOpts,
      ConcurrentAcceptorCount, AcceptorSup,
      {M,F,A} = OnStartup, OnShutdown, Label}) ->
	process_flag(trap_exit,true),
	case gen_tcp:listen(Port,SocketOpts ++ [{ip,IPAddress},{active,false}]) of %% active false means flow control on this
		{ok,LSock}->
			lists:foreach(fun(_)->{ok,_APid} = supervisor:start_child(AcceptorSup,[LSock]) end,
			lists:duplicate(ConcurrentAcceptorCount,dummy)),
			{ok,{LIPAddress,LPort}} = inet:sockname(LSock),
			error_logger:info_msg(
              "started ~s on ~s:~p~n",
              [Label, finch_netutil:ntoab(LIPAddress), LPort]),
		    apply(M,F,A ++ [IPAddress,Port]),
			{ok,#state{sock = LSock,on_startup = OnStartup,on_shutdown = OnShutdown,label=Label}};
		{error,Reason}->
			error_logger:error_msg(
              "failed to start ~s on ~s:~p - ~p~n",
              [Label, finch_netutil:ntoab(IPAddress), Port, Reason]),
		  	{stop,{cannot_listen,IPAddress,Port,Reason}}
	end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=LSock, on_shutdown = {M,F,A}, label=Label}) ->
    {ok, {IPAddress, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
    error_logger:info_msg("stopped ~s on ~s:~p~n",
                          [Label, tcp_server_util:ntoab(IPAddress), Port]),
    apply(M, F, A ++ [IPAddress, Port]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.