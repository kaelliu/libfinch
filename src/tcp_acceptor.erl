%%% File : tcp_acceptor.erl
%%% Description : acceptor of tcp connection
%%% Author : kael liu
%%% Date : 2012-08-01 Wednesday

-module(tcp_acceptor).
-behaviour(gen_server).
%% start_link
-export([start_link/2]).
%% callbacks of gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{callback,sock,ref}).

%%--------------------------------------------------------------------

start_link(Callback, LSock) ->
    gen_server:start_link(?MODULE, {Callback, LSock}, []).

%%--------------------------------------------------------------------

init({Callback,LSock})->
	% start accept here
	gen_server:cast(self(),accept),
	{ok,#state{callback=Callback,sock=LSock}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept,State)->
	accept(State);
handle_cast(_Msg,State)->
	{noreply,State}.

% accepted ok
handle_info({inet_async,LSock,Ref,{ok,Sock}},
			State = #state{callback={M,F,A},sock=LSock,ref=Ref})->
	%% patch up the socket so it looks like one we got from
	%% gen_tcp:accept/1
	%% {ok,Mod} = inet_db:lookup_socket(LSock),
	%% inet_db:register_socket(Sock,Mod),
	case set_sockopt(LSock, Sock) of
        ok -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
    end,
	try
		%% report
		{Address,Port} = inet_op(fun()->inet:sockname(LSock) end),
		{PeerAddress,PeerPort}= inet_op(fun()->inet:peername(Sock) end),
		error_logger:info_msg("accepted TCP connection on ~s:~p from ~s:~p~n",
                              [tcp_server_util:ntoab(Address), Port,
                               tcp_server_util:ntoab(PeerAddress), PeerPort]),
		%% handle
		apply(M,F,A++[Sock])
	catch {inet_error,_Reason}->
		gen_tcp:close(Sock)
	end,
	%% keep accept
	accept(State);
handle_info({inet_async, LSock, Ref, {error, closed}},
            State=#state{sock=LSock, ref=Ref}) ->
    %% It would be wrong to attempt to restart the acceptor when we
    %% know this will fail.
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------

set_sockopt(LSock, Sock) ->
    true = inet_db:register_socket(Sock, inet_tcp),
    case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(Sock, Opts) of
                ok    -> ok;
                Error -> 
                    gen_tcp:close(Sock),
                    Error
            end;
        Error ->
            gen_tcp:close(Sock),
            Error
    end.

inet_op(F) -> finch_netutil:throw_on_error(inet_error, F).

accept(State = #state{sock=LSock})->
	case prim_inet:async_accept(LSock,-1) of
		{ok,Ref}->{noreply,State#state{ref=Ref}};
		Error	->{stop,{cannot_accept,Error},State}
	end.

