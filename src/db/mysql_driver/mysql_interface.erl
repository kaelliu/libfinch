%%% File : mysql_interface.erl
%%% Description : mysql query interface
%%% Author : kael liu
%%% Date : 2013-03-19 Tuesday

-module(mysql_interface).

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/1,
        execute/2,
        transaction/1,
        select_limit/3,
        select_limit/4,
        get_one/1,
        get_one/2,
        get_row/1,
        get_row/2,
        get_all/1,
        get_all/2,
        make_insert_sql/3,
        make_update_sql/5]).

-include("common.hrl").

execute(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.
execute(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(?DB, Sql, Args) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end;
execute(Sql, Args) ->
    mysql:prepare(s, Sql),
    case mysql:execute(?DB, s, Args) of
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.

transaction(F) ->
    case mysql:transaction(?DB, F) of
        {atomic, R} -> R;
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Reason]);
        {aborted, {Reason, _}} -> mysql_halt([Reason]);
        Error -> mysql_halt([Error])
    end.

%% paged
select_limit(Sql, Offset, Num) ->
    S = list_to_binary([Sql, <<" limit ">>, integer_to_list(Offset), <<", ">>, integer_to_list(Num)]),
    case mysql:fetch(?DB, S) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.
select_limit(Sql, Args, Offset, Num) ->
    S = list_to_binary([Sql, <<" limit ">>, list_to_binary(integer_to_list(Offset)), <<", ">>, list_to_binary(integer_to_list(Num))]),
    mysql:prepare(s, S),
    case mysql:execute(?DB, s, Args) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.

%% get first column
get_one(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [[R]], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.
get_one(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(?DB, Sql, Args) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [[R]], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end;
get_one(Sql, Args) ->
    mysql:prepare(s, Sql),
    case mysql:execute(?DB, s, Args) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [[R]], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.

%% get first row
get_row(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.
get_row(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(?DB, Sql, Args) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end;
get_row(Sql, Args) ->
    mysql:prepare(s, Sql),
    case mysql:execute(?DB, s, Args) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.

get_all(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.
get_all(Sql, Args) when is_atom(Sql) ->
    case mysql:execute(?DB, Sql, Args) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end;
get_all(Sql, Args) ->
    mysql:prepare(s, Sql),
    case mysql:execute(?DB, s, Args) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.

%% generate sql like insert into users(field1,field2) values(a,b)
%% make_insert_sql(users,{field1,field2},{a,b})
make_insert_sql(Table, Field, Data) ->
    L = make_conn_sql(Field, Data, []),
    lists:concat(["insert into `", Table, "` set ", L]).

%% generate sql like insert into users(field1,field2) values(a,b,c) where key=k
%% make_insert_sql(users,{field1,field2},{a,b},uid,1).
make_update_sql(Table, Field, Data, Key, Value) ->
    L = make_conn_sql(Field, Data, []),
    lists:concat(["update `", Table, "` set ",L," where ",Key,"= '",sql_format(Value),"'"]).

sql_format(S) when is_integer(S)->
    integer_to_list(S);
sql_format(S) when is_float(S)->
    float_to_list(S);
sql_format(S) ->
    S.

make_conn_sql([], _, L ) ->
    L ;
make_conn_sql(_, [], L ) ->
    L ;
make_conn_sql([F | T1], [D | T2], []) ->
    L  = [F," = '",sql_format(D),"'"],
    make_conn_sql(T1, T2, L);
make_conn_sql([F | T1], [D | T2], L) ->
    L1  = L ++ [",", F," = '",sql_format(D),"'"],
    make_conn_sql(T1, T2, L1).

%% show readable log
mysql_halt([Sql, Reason]) ->
    erlang:error({db_error, [Sql, Reason]}).


