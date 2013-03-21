%%% File : tcp_client.erl
%%% Description : client job staff 
%%% Author : kael liu
%%% Date : 2012-08-01 Wednesday

-module(tcp_client).
%% start function
-export([start_link/0,init/1]).
-define(TCP_PARSE_TIME,1000).%% package parser time
-define(HEADER_LENGTH,4).%% package length 2 byte plus cmd length 2 byte
-define(HEART_TIMEOUT, 60000). % heart package timeout
-define(HEART_TIMEOUT_TIME, 0). % heart package timeout times

-record(linkinfo,{parent,sock,callback,buffer,connection_state}).
-record(client_info,{player=none,login=0,account_id=0,account_nm=none
					,timeout=0}).%% time_out_times

start_link()->
	% spawn a process for client logic
	{ok,proc_lib:spawn_link(?MODULE,init,[self()])}.

init(Parent)->
	receive
		{go,Sock,SockTransform}->
			start_connection(Parent,Sock,SockTransform)
	end.

%% get client data - login first
%% Socket：socket id
%% Client: client record
%% do_parse_login_package(Sock,Client)->
%% 	Ref = async_recv(Sock,?HEADER_LENGTH, ?HEART_TIMEOUT),%% read package first 4bytes
%% 	receive
%% 		%% 843 port for flash
%% %% 		{inet_async,Sock,Ref,{ok,?FL_POLICY_REQ}->
%% %% 			Len = 23 - ?HEADER_LENGTH,
%% %%             async_recv(Socket, Len, ?TCP_TIMEOUT),
%% %%             lib_send:send_one(Socket, ?FL_POLICY_FILE),
%% %%             gen_tcp:close(Socket);
%% 		{inet_async,Sock,Ref,{ok,<<Len:16,Cmd:16>>}}-> %% first two section no need be encrypt
%% 			BodyLen = Len - ?HEADER_LENGTH,			   %% bodyLen + 4 = Len
%% 			case BodyLen > 0 of
%% 				true->
%% 					Ref1 = async_recv(Sock,BodyLen,?TCP_PARSE_TIME),
%% 					receive
%% 						{inet_async,Sock,Ref1,{ok,Binary}}->
%% 							case logic_module:do_staff(package_parser:read(Binary)) of
								

switch_callback(State,Callback)->
	State#linkinfo{callback=Callback}.

start_connection(Parent,Sock,_SockTransform)->
	process_flag(trap_exit,true),
	Client = #client_info{player=none,login=0,account_id=0,account_nm=none,timeout=0},
	recv_loop(switch_callback(#linkinfo{parent=Parent,
							sock=Sock,
							callback = unknow_callback,
							buffer = []},
						handle_recv_msg)).

%% package_parse loop
do_package_parse(Sock,State)->
	Ref = async_recv(Sock, ?HEADER_LENGTH, ?HEART_TIMEOUT),
	receive
		{inet_async,Sock,Ref,{ok,<<L:16,Cmd:16>>}}->% header get
			BodyLen = L - ?HEADER_LENGTH,
			case BodyLen > 0 of
				true->
					Ref1 = async_recv(Sock, BodyLen, ?TCP_PARSE_TIME),
					receive
							{inet_async,Sock,Ref1,{ok,Binary}}->% data received
								void
						end;
				false->void
			end;
		{inet_async,Sock,Ref,{error,timeout}}->
			void;% timeout handler
		Other->
			void
	end.

recv_loop(State = #linkinfo{buffer = Buffer}) when Buffer =:= [] ->
		main_loop(State);
recv_loop(State = #linkinfo{buffer = Buffer})->
	Data = binary_to_list(Buffer),
	case logic_module:do_staff(package_parser:read(Data)) of
		{ok}->main_loop(State#linkinfo{buffer=[]});
		{error,reason}->error;
		{result_send_back,NewData}->
			package_parser:send(State#linkinfo.sock,NewData),
			main_loop(State#linkinfo{buffer=[]})
	end.

main_loop(State = #linkinfo{sock=Sock,buffer = Buffer})->
	%% active once for recv once,when operation is done,it will return faslse
	inet:setopts(Sock,[{active,once}]),
	case finch_netutil:recv(Sock) of
		{data,Data}->
			recv_loop(State#linkinfo{buffer=[Data|Buffer]});
		closed->
			handle_disconnected(State),exit(normal);
		{error,Reason}->
			throw({inet_error,Reason});
		{other,Other}->
			handle_other(Other,State)
	end.

%%% client close the connect
handle_disconnected(State)->
	{}.

handle_other(_Other,_State)->
	io:format("error msg~n").

%%% passive close the connect
handle_lost(Sock,Reason)->
	gen_tcp:close(Sock),
	exit({unexpected_message, Reason}).

%%% local function 
async_recv(Sock, Length, Timeout) when is_port(Sock)->
	case prim_inet:async_recv(Sock,Length,-1) of
		{error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
	end.		