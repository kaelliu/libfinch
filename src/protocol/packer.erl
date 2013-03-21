%%% File : packer.erl
%%% Description : pack tools for byte buffer
%%% Author : kael liu
%%% Date : 2012-08-01 Wednesday

-module(packer).
-export([pack/2,unpack/2]).
-export([byte/0,short/0,sshort/0,int/0,
		sint/0,long/0,slong/0]).
-export([list/2,tuple/1,record/2,binary/1,wstring/0]).
-export([string/0]).
-export([send/3]).
%%-include_lib("euint/include/eunit.hrl").

pack({PackFunc,_},Value)->
	lists:reverse(PackFunc([],Value)).

unpack({_,UnPackFunc},Bin)->
	element(1,UnPackFunc(Bin)).

%%% Base data struct 
%%% Byte
byte()->
	{fun write_byte/2,fun read_byte/1}.

write_byte(Bin,Byte)->
	[<<Byte:8>> | Bin].

read_byte(Bin)->
	<<Byte:8,Rest/binary>> = Bin,
	{Byte,Rest}.

%%% Unsigned short
short() -> 
    {fun write_short/2, fun read_short/1}.

write_short(Bin, Word) -> 
    [<<Word:16>>|Bin].

read_short(Bin) -> 
    <<Word:16, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Signed short

sshort() -> 
    {fun write_sshort/2, fun read_sshort/1}.

write_sshort(Bin, Word) -> 
    [<<Word:16/signed>>|Bin].

read_sshort(Bin) -> 
    <<Word:16/signed, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Unsigned int

int() -> 
    {fun write_int/2, fun read_int/1}.

write_int(Bin, Word) -> 
    [<<Word:32>>|Bin].

read_int(Bin) -> 
    <<Word:32, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Signed int

sint() -> 
    {fun write_sint/2, fun read_sint/1}.

write_sint(Bin, Word) -> 
    [<<Word:32/signed>>|Bin].

read_sint(Bin) -> 
    <<Word:32/signed, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Unsigned long

long() -> 
    {fun write_long/2, fun read_long/1}.

write_long(Bin, Word) -> 
    [<<Word:64>>|Bin].

read_long(Bin) -> 
    <<Word:64, Rest/binary>> = Bin,
    {Word, Rest}.

%%% Signed long

slong() -> 
    {fun write_slong/2, fun read_slong/1}.

write_slong(Bin, Word) -> 
    [<<Word:64/signed>>|Bin].

read_slong(Bin) -> 
    <<Word:64/signed, Rest/binary>> = Bin,
    {Word, Rest}.

%%% List
%%% param @LenFunc - ListLength
%%%       @Elem    - elem in list's apply
%%% return list pack/unpack function tuple
list(LenFunc,ElemFunc)->
	{fun(Bin,List)->write_list(LenFunc,ElemFunc,Bin,List) end,
	fun(Bin)->read_list(LenFunc,ElemFunc,Bin) end}.

write_list({LenFunc,_},{ElemFunc,_},Bin,List)->
	Bin1 = LenFunc(Bin,length(List)),
	Fun = fun(A,Bin2)->ElemFunc(Bin2,A) end,
	lists:foldr(Fun,Bin1,List).

read_list({_,LenFunc},{_,ElemFunc},Bin)->
	{N,Bin1} = LenFunc(Bin),
	read_list(N,[],ElemFunc,Bin1).
read_list(0,Acc,_,Bin)->{Acc,Bin};
read_list(N,Acc,ElemFunc,Bin)->
	{E,Bin1}=ElemFunc(Bin),
	read_list(N-1,[E|Acc],ElemFunc,Bin1).

%%% Optional value. Use 'none' to indicate no value.
optional(PackFunc)->
	{
		fun(Acc,Value)->write_optional(PackFunc,Acc,Value) end,
		fun(Bin)->read_optional(PackFunc,Bin) end
	}.

write_optional(_,Acc,none)->
	[<<0>>|Acc];
write_optional({PackFunc,_},Acc,Value)->
	PackFunc([<<1>>|Acc],Value).

read_optional({_,UnPackFunc},Bin)->
	<<Opt:8,Bin1/binary>> = Bin,
	case Opt of
		0->{none,Bin1};
		_->UnPackFunc(Bin1)
	end.

%%% Extra data struct
%%% Wrapper. Take a pack and a wrapper tuple of two functions
%%% where the first one is used to convert the value before 
%%% packing and the second one after unpacking.
%%% wrap data struct like tuple,list
wrap(WrapFun,PackFun)->
	{fun(Acc,Value)->write_wrap(WrapFun,PackFun,Acc,Value) end,
	 fun(Bin)->read_wrap(WrapFun,PackFun,Bin) end}.

write_wrap({WrapFun,_},{PackFun,_},Acc,Value)->
	PackFun(Acc,WrapFun(Value)).

read_wrap({_,WrapFun},{_,PackFun},Bin)->
	{Value,Bin1} = PackFun(Bin),
	{WrapFun(Value),Bin1}.

%%% Enumerated values start from 1 for the tuple case.
prep_enum_tuple(Enum) when is_tuple(Enum)->
	prep_enum_tuple(Enum,size(Enum),[],[]).
prep_enum_tuple(_,0,Acc1,Acc2)->
	{Acc1,Acc2};
prep_enum_tuple(Enum,N,Acc1,Acc2)->
	prep_enum_tuple(Enum,N-1,[{element(N,Enum),N}|Acc1],[{N,element(N,Enum)}|Acc2]).

% expect a list of {tag, #value} pairs
prep_enum_list(Enum) when is_list(Enum)->
	Inv = fun({Key,Val})->{Val,Key} end,
	InvEnum = lists:map(Inv, Enum),
	{Enum,InvEnum}.

wrap_enum(Enum) when is_tuple(Enum)->
	wrap_enum_1(prep_enum_tuple(Enum));
wrap_enum(Enum) when is_list(Enum)->
	wrap_enum_1(prep_enum_list(Enum)).

wrap_enum_1({List1,List2})->
	F = fun(A,B)-> A < B end,
	%% gb_trees needs an ordered list
	Dict1 = lists:sort(F, List1),
	Dict2 = lists:sort(F, List2),
	Tree1 = gb_trees:from_orddict(Dict1),
	Tree2 = gb_trees:from_orddict(Dict2),
	{fun(Key)->gb_trees:get(Key, Tree1) end,
	 fun(Key)->gb_trees:get(Key, Tree2) end}.

enum(Enum,PackFun)->
	wrap(wrap_enum(Enum),PackFun).

%%% Tuple. Uses a tuple of packer of the same size.
%%% like tuple{"kael",16#00ff,none},you need a pack/unpack functionlist
%%% Spec = {list(byte(),byte()),short(),optional(byte())}
tuple(PackFun) when is_tuple(PackFun)->
	wrap({fun tuple_to_list/1,fun list_to_tuple/1},tuple_0(tuple_to_list(PackFun))).

%%% same as tuple
%%% Record. We rely on Erlang records being tuples
%%% and just add the record tag as the first element
%%% when unpacking the record.
record(Tag,PackFun) when is_tuple(PackFun)->
	wrap({fun(Record)->record_to_list(Tag,Record) end,
		  fun(List)->list_to_record(Tag,List) end}
		,tuple_0(tuple_to_list(PackFun))).

write_tuple_0([],Acc,_)->
	Acc;
write_tuple_0([{PackFun,_}|Rest],Acc,[Value|Tuple])->
	write_tuple_0(Rest,PackFun(Acc,Value),Tuple).

read_tuple_0(PackFun,Bin)->
	read_tuple_0(PackFun,Bin,[]).
read_tuple_0([],Bin,Acc)->
	{lists:reverse(Acc),Bin};
read_tuple_0([{_,PackFun}|Rest],Bin,Acc)->
	{Value,Bin1} = PackFun(Bin),
	read_tuple_0(Rest,Bin1,[Value|Acc]).

%%% It's convenient to be able to convert the tuple
%%% to a list first as there's no erlang:prepend_element/2.
tuple_0(PackFun) when is_list(PackFun)->
	{fun(Acc,Value)->write_tuple_0(PackFun,Acc,Value) end,
	 fun(Bin)->read_tuple_0(PackFun,Bin) end}.

record_to_list(Tag,Record) when is_atom(Tag)->
	lists:nthtail(1, tuple_to_list(Record)).

list_to_record(Tag,List)when is_atom(Tag),is_list(List)->
	list_to_tuple([Tag|List]).

%%% Binary
binary(Size)->
	{fun(Acc,Bin)->write_binary(Size,Acc,Bin) end,
	 fun(Bin)->read_binary(Size,Bin) end}.

write_binary({Size,_},Acc,undefined)->
	Size(Acc,0);
write_binary({Size,_},Acc,Bin)->
	Acc1 = Size(Acc,size(Bin)),
	[Bin|Acc1].

read_binary({_,Size},Bin)->
	{N,Bin1} = Size(Bin),
	<<Value:N/binary,Bin2/binary>> = Bin1,
	{Value,Bin2}.

%%% Wide string, little-endian style

wstring() ->
    {fun write_wstring/2, 
     fun read_wstring/1}.

write_wstring(Acc, []) ->
    [<<0:16>>|Acc];

write_wstring(Acc, [H|T]) ->
    write_wstring([<<H:16>>|Acc], T).

read_wstring(Bin) ->
    read_wstring(Bin, []).

read_wstring(<<0:16, Bin/binary>>, Acc) ->
    {lists:reverse(Acc), Bin};

read_wstring(<<X:16, Bin/binary>>, Acc) ->
    read_wstring(Bin, [X|Acc]).

string() ->    
    binary(byte()).

%%% misc
binary_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.

send(Sock,Data,Wrapper)->
	Bin = list_to_binary(Wrapper(Data)),
	case catch gen_tcp:send(Sock,Bin) of
		ok->
			ok;
		{error,closed}->
			ok;
		{error,econnaborted}->
			ok;
		Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send error"},
                                       {module, ?MODULE}, 
                                       {line, ?LINE},
                                       {socket, Sock}, 
                                       {port_info, erlang:port_info(Sock, connected)},
                                       {data, Data},
                                       {bin, Bin},
                                       {error, Any}
                                      ])
	end.