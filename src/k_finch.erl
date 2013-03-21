%%% File : k_finch.erl
%%% Description : application manager
%%% Author : kael liu
%%% Date : 2013-03-19 Tuesday

-module(k_finch).

%% ====================================================================
%% API functions
%% ====================================================================
-export([game_server_start/0,game_server_stop/0,init_ets/0,sys_info/0]).
-define(SERVER_APPS,[sasl,finch_server_app]).
-define(OTHER_SERVER_APP,[sasl,xxx_server_app]).

game_server_start()->
	try
		ok = start_applications(?SERVER_APPS)
	after
		timer:sleep(100)
	end.

game_server_stop()->
	ok = stop_applications(?SERVER_APPS),
	erlang:halt().

init_ets()->
	ok.

sys_info()->
	SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    io:format( "abormal termination:
                       ~n   Scheduler id:                         ~p
                       ~n   Num scheduler:                        ~p
                       ~n   Process count:                        ~p
                       ~n   Process limit:                        ~p
                       ~n   Memory used by erlang processes:      ~p
                       ~n   Memory allocated by erlang processes: ~p
                       ~n   The total amount of memory allocated: ~p
                       ~n",
                            [SchedId, SchedNum, ProcCount, ProcLimit,
                             ProcMemUsed, ProcMemAlloc, MemTot]),
      ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
manage_applications(Iter,Do,Undo,SkipError,ErrorTag,Apps)->
	Iter(fun (App,Acc)->
				  case Do(App) of
					  ok -> [App|Acc];
					  {error,{SkipError,_}}->Acc;
					  {error,Reason}->
						  lists:foreach(Undo, Acc),
						  throw({error, {ErrorTag, App, Reason}})
				  end
		 end,[],Apps),
	ok.

start_applications(Apps)->
	manage_applications(fun lists:foldl/3,
						fun application:start/1,
						fun application:stop/1,
						already_started,
						cannot_start_application,
						Apps).

stop_applications(Apps)->
	manage_applications(fun lists:foldl/3,
						fun application:stop/1,
						fun application:start/1,
						not_started,
						cannot_stop_application, Apps).