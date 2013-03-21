%%% File : prof.erl
%%% Description : simple profiler
%%% Author : kael liu
%%% Date : 2013-03-19 Tuesday
-module(prof).
-compile(export_all).

%% 
run(Fun, Loop) -> 
    statistics(wall_clock),%% start time
    for(1, Loop, Fun),
    {_, T1} = statistics(wall_clock),%% over time
    io:format("~p loops, using time: ~pms~n", [Loop, T1]),
    ok.

%% for implement
for(Max, Max , Fun) ->
    Fun();
for(I, Max, Fun) -> 
    Fun(), for(I + 1, Max, Fun).
        
    
