%%% File : id_helper.erl
%%% Description : id operation 
%%% Author : kael liu
%%% Date : 2012-08-01 Wednesday

-module(id_helper).

-export([pid2key/1, pid2id/1, key2id/1]).

pid2key(Pid) when is_pid(Pid) ->
    {erlang:phash2(now(), 1 bsl 32),
     erlang:phash2(Pid, 1 bsl 32)}.

key2id(Key) 
  when is_tuple(Key), size(Key) == 2 ->
    erlang:phash2(Key, 1 bsl 32).

pid2id(Pid) when is_pid(Pid) ->
    key2id(pid2key(Pid)).

