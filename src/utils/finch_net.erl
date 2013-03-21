%%% File : finch_net.erl
%%% Description : finch net util
%%% Author : kael liu
%%% Date : 2012-07-31 Tuesday

-module(finch_net).
-export([controlling_process/2,getstat/2,
		recv/1,async_recv/3,port_command/2,getopts/2,setopts/2,send/2,
		close/1,maybe_fast_close/1,sockname/1,peername/1,tune_buffer_size/1]).

%%---------------------------------------------------------------------------

-ifdef(use_specs).
%% A module can export some types in order to declare that other modules are allowed
%% to refer to them as remote types. 
-export_type([socket/0]).

-type(stat_option() ::
        'recv_cnt' | 'recv_max' | 'recv_avg' | 'recv_oct' | 'recv_dvi' |
        'send_cnt' | 'send_max' | 'send_avg' | 'send_oct' | 'send_pend').
-type(socket() :: port()).
-type(opts() :: [{atom(), any()} |
                 {raw, non_neg_integer(), non_neg_integer(), binary()}]).

-spec(controlling_process/2 :: (socket(), pid()) -> ok | error).
-spec(getstat/2 ::
        (socket(), [stat_option()])
        -> ok | error).
-spec(recv/1 :: (socket()) ->
                     {'data', [char()] | binary()} | 'closed' |
                     error | {'other', any()}).
-spec(async_recv/3 ::
        (socket(), integer(), timeout()) -> ok).
-spec(port_command/2 :: (socket(), iolist()) -> 'true').
-spec(getopts/2 :: (socket(), [atom() | {raw,
                                         non_neg_integer(),
                                         non_neg_integer(),
                                         non_neg_integer() | binary()}])
                   -> ok | error).
-spec(setopts/2 :: (socket(), opts()) -> ok | error).
-spec(send/2 :: (socket(), binary() | iolist()) -> ok | error).
-spec(close/1 :: (socket()) -> ok | error).
-spec(maybe_fast_close/1 :: (socket()) -> ok | error).
-spec(sockname/1 ::
        (socket())
        -> {ok,val} | {error,Reason}).
-spec(peername/1 ::
        (socket())
        -> {ok,val} | {error,Reason}).
-spec(tune_buffer_size/1 :: (socket()) -> ok | error).

-endif.

%%---------------------------------------------------------------------------

%%% change controlling of a socket to a process
controlling_process(Sock,Pid) when is_port(Sock)->
	gen_tcp:controlling_process(Sock,Pid).

getstat(Sock,Stats) when is_port(Sock)->
	inet:getstat(Sock,Stats).

recv(Sock) when is_port(Sock)->
	recv(Sock,{tcp,tcp_closed,tcp_error}).

recv(Sock,{DataTag,ClosedTag,ErrorTag})->
	receive
		{DataTag,S,Data}	  -> {data,Data};
		{ClosedTag, S}        -> closed;
        {ErrorTag, S, Reason} -> {error, Reason};
        Other                 -> {other, Other}
	end.

async_recv(Sock,Length,infinity) when is_port(Sock)->
	prim_inet:async_recv(Sock,Length,-1);
async_recv(Sock,Length,Timeout) when is_port(Sock)->
	prim_inet:async_recv(Sock,Length,Timeout).

%% Sends data to a port.
port_command(Sock,Data) when is_port(Sock)->
	erlang:port_command(Sock,Data).

getopts(Sock,Options) when is_port(Sock)->
	inet:getopts(Sock,Options).

setopts(Sock,Options) when is_port(Sock)->
	inet:setopts(Sock,Options).

send(Sock,Data) when is_port(Sock)->
	gen_tcp:send(Sock,Data).

close(Sock) when is_port(Sock)->
	gen_tcp:close(Sock).

maybe_fast_close(Sock) when is_port(Sock)->
	erlang:port_close(Sock),ok.

%% Returns the local address and port number for a socket
sockname(Sock) when is_port(Sock)->
	inet:sockname(Sock).

%% Returns the address and port for the other end of a connection.
peername(Sock) when is_port(Sock)->
	inet:peername(Sock).

tune_buffer_size(Sock)->
	case getopts(Sock,[sndbuf,recbuf,buffer]) of
		{ok,BufSizes}-> BufSz = lists:max([Sz || {_Opt,Sz}<-BufSizes]),
					setopts(Sock,[{buffer,BufSz}]);
		Err			-> Err
	end.
