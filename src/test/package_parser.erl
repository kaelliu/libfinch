%%% File : package_parser.erl
%%% Description : binrary data parser usage test
%%% Author : kael liu
%%% Date : 2012-08-01 Wednesday

-module(package_parser).
-include("package_parser.hrl").
-export([read/2,write/1,send_data/2]).
-import(
		packer, [pack/2, unpack/2, byte/0, 
                 short/0, sshort/0, int/0, sint/0, 
                 long/0, slong/0, list/2, 
                 optional/1, wrap/2, tuple/1, record/2, 
                 binary/1, string/0, wstring/0,send/3
                ]).

internal()->
	{fun(Acc,_)->Acc end,
	 fun(Bin)->{undefined,Bin} end}.

year() ->
    int().

month() ->
    byte().

day() ->
    byte().

date_() ->
    tuple({year(), month(), day()}).

hour() ->
    byte().

minute() ->
    byte().

second() ->
    byte().

time_() ->
    tuple({hour(), minute(), second()}).

datetime() ->
    tuple({date_(), time_()}).

timestamp() ->
    tuple({int(), int(), int()}).

host() ->
    string().

port() ->
    short().

%%% Commands 
bad()->
	record(bad,{short(),byte()}).

login()->
	record(login,{string(),string()}).

write(R) when is_record(R,bad)->
	write(?CMD_BAD,pack(bad(),R));
write(R) when is_record(R,login)->
	write(?CMD_LOGIN,pack(login(),R)).

%% write(Cmd,R,Tag) when is_record(R, bad)->
%% 	write(Cmd,pack(Tag,R)).

write(Cmd,Data)->
	L = byte_size(Data),
	<<L:16,Cmd:16,Data/binary>>.

%% seems a lot...
read(?CMD_BAD,Bin)->
	unpack(bad(),Bin);
read(?CMD_LOGIN,Bin)->
	unpack(login(),Bin).
%% read0(<<PackLen:16,Bin/binary>>)
%%     when PackLen =:= byte_size(Bin)->
%%				read(Bin).
%% encrypt(Data,_BinFunc)->
%% dencrypt(Data,_BinFunc)->
send_data(Sock,Data)->
	send(Sock,Data,fun(X)->write(X) end).

