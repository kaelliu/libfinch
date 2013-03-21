%%% File : finch_utils.erl
%%% Description : common util
%%% Author : kael liu
%%% Date : 2013-03-19 Tuesday

-module(finch_utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([thing_to_list/1,make_log/5,rand/2,unixtime/0,
		 longunixtime/0,for/3,for/4,get_list/2,sleep/1,
		 sleep/2,floor/1,ceil/1,md5_hex/1,string_to_term/1,
        bitstring_to_term/1,
        term_to_string/1,
        term_to_bitstring/1]).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

make_log(T,F,A,Mod,Line)->
	{ok,Fd} = file:open("logs/err.log", [write,append]),
	Format  = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
	{{Y,M,D},{H,I,S}} = erlang:localtime(),
	Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
	io:format(Fd, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
	file:close(Fd).

unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

longunixtime() ->
    {M, S, Ms} = erlang:now(),
    M * 1000000000 + S*1000 + Ms div 1000.

% for looping
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

% for looping with state
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

get_list([], _) ->
    [];
get_list(X, F) ->
    F(X).

sleep(T) ->
    receive
    after T -> ok
    end.

 sleep(T, F) ->
    receive
    after T -> F()
    end.

% floor(5.5) -> 5
% floor(-5.5) -> -6
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.
% ceil(5.5) -> 6
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

md5_hex(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

rand(Same, Same) -> Same;
rand(Min, Max) ->
	case get("rand_seed") of
        undefined ->
            RandSeed = unixtime(),
            random:seed(RandSeed),
            put("rand_seed", RandSeed);
        _ -> skip
    end,
	M = Min - 1,
    random:uniform(Max - M) + M.

% [a,{1}] -> "[a,{1}]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).
% [a,{1}] -> <<"[a,{1}]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).
% "[a,{1}]"-> [a,{1}]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.
% <<"[a,{1}]">> -> [a,{1}]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).
%% ====================================================================
%% Internal functions
%% ====================================================================


