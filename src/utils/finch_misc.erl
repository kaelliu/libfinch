%%% File : finch_misc.erl
%%% Description : misc funcs
%%% Author : kael liu
%%% Date : 2012-07-31 Tuesday

-module(finch_misc).
-export([method_record_type/1]).
-export([polite_pause/1,dirty_read/1,table_lookup/2,set_table_value/4]).
-export([pid_to_string/1,string_to_pid/1]).
-export([version_compare/2, version_compare/3]).
-export([gb_trees_cons/3,now_ms/0]).
-export([gb_trees_fold/3, gb_trees_foreach/2]).
-export([const_ok/0, const/1]).
-export([is_process_alive/1,os_cmd/1,get_timestamp/0]).
%% get record's section 1
method_record_type(Record)->
	element(1,Record).

%% pause process for time N
polite_pause()->
	polite_pause(3000).
polite_pause(N)->
	receive
	after N-> done
	end.

dirty_read({Table,Key})->
	case ets:lookup(Table,Key) of
		[Result] -> {ok,Result};
		[]->{error,not_found}
	end.

table_lookup(Table,Key)->
	case lists:keysearch(Key,1,Table) of
		{value,{_,TypeBin,ValueBin}}-> {TypeBin,ValueBin};
		false					    -> undefined
	end.

set_table_value(Table, Key, Type, Value) ->
    lists:keysort(1,
		lists:keystore(Key, 1, Table, {Key, Type, Value})).

%% This provides a string representation of a pid that is the same
%% regardless of what node we are running on. The representation also
%% permits easy identification of the pid's node.
pid_to_string(Pid) when is_pid(Pid) ->
    %% see http://erlang.org/doc/apps/erts/erl_ext_dist.html (8.10 and
    %% 8.7)
    <<131,103,100,NodeLen:16,NodeBin:NodeLen/binary,Id:32,Ser:32,Cre:8>>
        = term_to_binary(Pid),
    Node = binary_to_term(<<131,100,NodeLen:16,NodeBin:NodeLen/binary>>),
    format("<~w.~B.~B.~B>", [Node, Cre, Id, Ser]).

format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

%% inverse of above
string_to_pid(Str) ->
    Err = {error, {invalid_pid_syntax, Str}},
    %% The \ before the trailing $ is only there to keep emacs
    %% font-lock from getting confused.
    case re:run(Str, "^<(.*)\\.(\\d+)\\.(\\d+)\\.(\\d+)>\$",
                [{capture,all_but_first,list}]) of
        {match, [NodeStr, CreStr, IdStr, SerStr]} ->
            %% the NodeStr atom might be quoted, so we have to parse
            %% it rather than doing a simple list_to_atom
            NodeAtom = case erl_scan:string(NodeStr) of
                           {ok, [{atom, _, X}], _} -> X;
                           {error, _, _} -> throw(Err)
                       end,
            <<131,NodeEnc/binary>> = term_to_binary(NodeAtom),
            [Cre, Id, Ser] = lists:map(fun list_to_integer/1,
                                       [CreStr, IdStr, SerStr]),
            binary_to_term(<<131,103,NodeEnc/binary,Id:32,Ser:32,Cre:8>>);
        nomatch ->
            throw(Err)
    end.

version_compare(A, B, lte) ->
    case version_compare(A, B) of
        eq -> true;
        lt -> true;
        gt -> false
    end;
version_compare(A, B, gte) ->
    case version_compare(A, B) of
        eq -> true;
        gt -> true;
        lt -> false
    end;
version_compare(A, B, Result) ->
    Result =:= version_compare(A, B).

version_compare(A, A) ->
    eq;
version_compare([], [$0 | B]) ->
    version_compare([], dropdot(B));
version_compare([], _) ->
    lt; %% 2.3 < 2.3.1
version_compare([$0 | A], []) ->
    version_compare(dropdot(A), []);
version_compare(_, []) ->
    gt; %% 2.3.1 > 2.3
version_compare(A,  B) ->
    {AStr, ATl} = lists:splitwith(fun (X) -> X =/= $. end, A),
    {BStr, BTl} = lists:splitwith(fun (X) -> X =/= $. end, B),
    ANum = list_to_integer(AStr),
    BNum = list_to_integer(BStr),
    if ANum =:= BNum -> version_compare(dropdot(ATl), dropdot(BTl));
       ANum < BNum   -> lt;
       ANum > BNum   -> gt
    end.

dropdot(A) -> lists:dropwhile(fun (X) -> X =:= $. end, A).

gb_trees_cons(Key, Value, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Values} -> gb_trees:update(Key, [Value | Values], Tree);
        none            -> gb_trees:insert(Key, [Value], Tree)
    end.

gb_trees_fold(Fun, Acc, Tree) ->
    gb_trees_fold1(Fun, Acc, gb_trees:next(gb_trees:iterator(Tree))).

gb_trees_fold1(_Fun, Acc, none) ->
    Acc;
gb_trees_fold1(Fun, Acc, {Key, Val, It}) ->
    gb_trees_fold1(Fun, Fun(Key, Val, Acc), gb_trees:next(It)).

gb_trees_foreach(Fun, Tree) ->
    gb_trees_fold(fun (Key, Val, Acc) -> Fun(Key, Val), Acc end, ok, Tree).

now_ms() ->
    timer:now_diff(now(), {0,0,0}) div 1000.

const_ok() -> ok.
const(X) -> fun () -> X end.

is_process_alive(Pid) when node(Pid) =:= node() ->
    erlang:is_process_alive(Pid);
is_process_alive(Pid) ->
    case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
        true -> true;
        _    -> false
    end.

os_cmd(Command) ->
    Exec = hd(string:tokens(Command, " ")),
    case os:find_executable(Exec) of
        false -> throw({command_not_found, Exec});
        _     -> os:cmd(Command)
    end.

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.