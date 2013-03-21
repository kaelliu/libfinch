%%%---------------------------------------------

-module(mysql_test).
-compile(export_all).
-include("emysql.hrl").
-define(DB_POOLID, k_mysql_conn).
-define(DB_HOST, "localhost").
-define(DB_PORT, 3306).
-define(DB_USER, "root").
-define(DB_PASS, "2932615").
-define(DB_NAME, "spd").
-define(DB_ENCODE, utf8).
-define(DB_NAME2,"dz").
-record(test,{id,abc,g}).
-record(state,{running=0,start_time,process_count_all,sql_count_each_process}).

conn_emysql()->
	crypto:start(),
	emysql:start(),
	emysql:add_pool(?DB_POOLID, 1, ?DB_USER, ?DB_PASS, ?DB_HOST, ?DB_PORT, ?DB_NAME2, ?DB_ENCODE),
	emysql:execute(?DB_POOLID, io_lib:format(<<"INSERT INTO test(abc,g) values('~s',~p)">>, ["123",345])),
	Result = emysql:execute(?DB_POOLID, <<"select abc,g from test">>),
	Recs = emysql_util:as_record(Result,test, record_info(fields,test)),
	Abcs = [Test#test.abc || Test<-Recs].
%% 	io:format("~n~p~n", Abcs).

test_emysql()->
	crypto:start(),
	emysql:start(),
	emysql:add_pool(?DB_POOLID, 10, ?DB_USER, ?DB_PASS, ?DB_HOST, ?DB_PORT, ?DB_NAME2, ?DB_ENCODE),
	F = fun() ->
		%% linear
        emysql:execute(?DB_POOLID, io_lib:format(<<"INSERT INTO test(abc,g) values('~s',~p)">>, ["123",345]))
    end,
    prof:run(F, 10000),
	ok.
	
%% spawn process,do every query work per process
start(ProcessCount,SqlCountEachProcess,MysqlConnectionCount)->
    crypto:start(),
    emysql:start(),
    emysql:add_pool(?DB_POOLID, MysqlConnectionCount, ?DB_USER, ?DB_PASS, ?DB_HOST, 3306, ?DB_NAME2, utf8),

    CurrentTime=finch_misc:get_timestamp(),
    spawn_link(?MODULE,run,[ProcessCount,SqlCountEachProcess,#state{start_time=CurrentTime,
                                                                    process_count_all=ProcessCount,
                                                                    sql_count_each_process=SqlCountEachProcess}]).

recv(#state{running=0,start_time=StartTime,
            process_count_all=ProcessCount,
            sql_count_each_process=SqlCountEachProcess
           })->
    Usedtime = finch_misc:get_timestamp()-StartTime,
    io:format("process_count:~p sql count each process:~p used time:~p~n",[ProcessCount,SqlCountEachProcess,Usedtime]),
    emysql:remove_pool(hello_pool);
recv(#state{running=Running}=State)->
    receive
        done->
			io:format("done! ~p~n",[State#state.running]),
            recv(State#state{running=Running-1})
    end
        .

run(0,_SqlCountEachProcess,#state{}=State)->
    recv(State);
run(ProcessCount,SqlCountEachProcess,#state{running=Running}=State) ->
    Parent =self(),
    spawn(fun()-> run_sql(SqlCountEachProcess,Parent)end),
    io:format("running process ~p~n",[State#state.running]),
    run(ProcessCount-1,SqlCountEachProcess,State#state{running=Running+1}).

run_sql(0,Parent)->
    Parent!done;
run_sql(SqlCountEachProcess,Parent) ->
    test2(),
    run_sql(SqlCountEachProcess-1 ,Parent)
        .
test2()->
    Result=emysql:execute(?DB_POOLID, io_lib:format(<<"INSERT INTO test(abc,g) values('~s',~p)">>, ["123",345])),
    case Result of
        Rec when is_record(Rec ,ok_packet) ->
            ok;
        Rec when is_record(Rec ,error_packet) ->
            io:format("~p~n",[Result])
    end

    %% %% Pid=global:whereis_name(emysql_center)
        .

conn()->
    mysql:start_link(?DB_POOLID, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME2, fun(_, _, _, _) -> ok end, ?DB_ENCODE),
    mysql:connect(?DB_POOLID, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME2, ?DB_ENCODE, true),
%    mysql:fetch(?DB, <<"drop table if exists test">>),
%    mysql:fetch(?DB, <<"create table test (id int not null auto_increment,row varchar(50) not null,r int not null, primary key (id)) engine = myisam">>),
    mysql:fetch(?DB_POOLID, <<"truncate table test">>),
    ok.

test() ->
    %mysql:fetch(?DB, <<"truncate table test">>),
    mysql:fetch(?DB_POOLID, <<"begin">>),
    F = fun() ->
        mysql_interface:execute(io_lib:format(<<"insert into  `test` (`row`,`r`) values ('~s',~p)">>,["123",123]))
%%         mysql_interface:execute(io_lib:format(<<"update  `test` set  `row` = '~s' where id = ~p">>,["456�",1]))
%        mysql:fetch(?DB, io_lib:format(<<"insert into  `test` (`row`,`r`) values ('~s',~p)">>,["789",123]))
    end,
    prof:run(F, 10000),
    mysql:fetch(?DB_POOLID, <<"commit">>),

%    mysql:fetch(?DB, <<"begin">>),
%
%    F1 = fun() ->
%        mysql:fetch(?DB, io_lib:format(<<"update  `test` set  `row` = '~s' where id = ~p">>,["我是来测试性能的",123]))
%    end,
%    prof:run(F1, 10000),
%    mysql:fetch(?DB, <<"commit">>),
%
%    F2 = fun() ->
%        mysql:fetch(?DB, <<"select * from  `test` where id = 1">>)
%    end,
%    prof:run(F2, 10000),

ok.

