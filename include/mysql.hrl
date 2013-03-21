%%% File : mysql.hrl
%%% Description : mysql query result record
%%% Author : kael liu
%%% Date : 2013-03-19 Tuesday
-record(mysql_result,{fieldinfo=[],rows=[],affectedrows=0,error=""}).