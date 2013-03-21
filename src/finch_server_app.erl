%%% File : finch_server_app.erl
%%% Description : program entry
%%% Author : kael liu
%%% Date : 2012-08-03 Friday

-module(finch_server_app).
-behaviour(application).
-include("common.hrl").
-export([start/2,stop/1]).

start(normal,[])->
	%% start the finch_sup here,finch_sup is the main process to monitor the sub process
	%% get the ip:port by startup param
%%  	[Ip, Port, _Sid] = init:get_plain_arguments(),
	{ok,SupPid} = finch_sup:start_link(),
	finch_networking:boot(),
	{ok,SupPid}.

stop(_State)->
	void.