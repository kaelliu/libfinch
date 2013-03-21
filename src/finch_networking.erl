%%% File : finch_networking.erl
%%% Description : network_program startup
%%% Author : kael liu
%%% Date : 2012-07-31 Tuesday

-module(finch_networking).
-export([boot/0,start/0,start_tcp_listener/1,stop_tcp_listener/1,unboot/0]).
-export([tcp_listener_spec/6]).
%%-include("kernel/include/inet.hrl").
% outside send in..
-include("common.hrl").
-define(MAX_RESTART_TIMES,5).
-define(MAX_TIME,30).

%% close entry
unboot()->
	%% db pool destory/other module destory
	stop_tcp_listener(?DEFAULT_PORT).

%% start entry
boot()->
	%% todo:do some print,should database start here?
	%% start client sup and listener sup both
	ok=start(),
	ok=boot_tcp().

%% tcp listener sup init
boot_tcp()->
	% you can put {env,[{key,value},{k2,v2}]} in app file
    % data like key-value and can be read by application:get_env/2
	% TcpListeners is port list for example,{env,[{tcp_listeners,[1234,4567,8901]}]}
	case application:get_env(tcp_listeners) of
		undefined->ok=start_tcp_listener(?DEFAULT_PORT);
		{ok,TcpListeners}->
			[ok=start_tcp_listener(Listener) || Listener <- TcpListeners]
	end,
	ok.

%% tcp client sup init
start()->
	finch_sup:start_supervisor_child(
		finch_tcp_client_sup,tcp_client_sup,
		{tcp_client,start_link,[]}).%% child client connection
		%%[{local,tcp_client_sup},{finch_connection_sup,start_link,[]}]).

%% get the {ipaddress,port,family} tuple back for tcp_listener startup
tcp_listener_addresses(Port) when is_integer(Port)->
	tcp_listener_addresses_auto(Port);
tcp_listener_addresses({"auto", Port}) ->
    %% Variant to prevent lots of hacking around in bash and batch files
    tcp_listener_addresses_auto(Port);
tcp_listener_addresses({Host, Port}) ->
    %% auto: determine family IPv4 / IPv6 after converting to IP address
    tcp_listener_addresses({Host, Port, auto});
tcp_listener_addresses({Host, Port, Family0})
  when is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) ->
    [{IPAddress, Port, Family} ||
        {IPAddress, Family} <- inet:getaddr(Host, Family0)];
tcp_listener_addresses({_Host, Port, _Family0}) ->
    error_logger:error_msg("invalid port ~p - not 0..65535~n", [Port]),
    throw({error, {invalid_port, Port}}).

tcp_listener_addresses_auto(Port) ->
    lists:append([tcp_listener_addresses(Listener) ||
                     Listener <- port_to_listeners(Port)]).

%% the tcp listener config 
tcp_listener_spec(NamePrefix, {IPAddress, Port, Family}, SocketOpts,
                  Protocol, Label, OnConnect) ->
    {finch_netutil:tcp_name(NamePrefix, IPAddress, Port),   %% listener id,listenerprocess_ipaddress:port
     {tcp_listener_sup, start_link,
      [IPAddress, Port, [Family | SocketOpts],
       {?MODULE, tcp_listener_started, [Protocol]},			%% on listener starts
       {?MODULE, tcp_listener_stopped, [Protocol]},         %% on listener shutdown
       OnConnect, Label]},									%% mfa
     transient, infinity, supervisor, [tcp_listener_sup]}.
		 
%% start the tcp listener process
start_tcp_listener(Listener) ->
    start_listener(Listener, bin_protocol, "TCP Listener",
                   {?MODULE, start_client, []}).% <-- onConnect is {finch_networking,start_client,[]} 
start_listener(Listener, Protocol, Label, OnConnect) ->
    [start_listener0(Address, Protocol, Label, OnConnect) ||
        Address <- tcp_listener_addresses(Listener)],
    ok.
start_listener0(Address, Protocol, Label, OnConnect) ->
    Spec = tcp_listener_spec(finch_tcp_listener_sup, Address, tcp_opts(),%% get from app file
                             Protocol, Label, OnConnect),
    case supervisor:start_child(finch_sup, Spec) of % start as finch_sup's child
        {ok, _}                -> ok;
        {error, {shutdown, _}} -> {IPAddress, Port, _Family} = Address,
                                  exit({could_not_start_tcp_listener,
                                        {finch_netutil:ntoa(IPAddress), Port}})
    end.


stop_tcp_listener(Listener) ->
    [stop_tcp_listener0(Address) ||
        Address <- tcp_listener_addresses(Listener)],
    ok.
stop_tcp_listener0({IPAddress, Port, _Family}) ->
    Name = finch_netutil:tcp_name(finch_tcp_listener_sup, IPAddress, Port),
    ok = supervisor:terminate_child(finch_sup, Name),
    ok = supervisor:delete_child(finch_sup, Name).

tcp_listener_started(_Protocol, IPAddress, Port) ->
  	io:format("[~p:~p] listerner started~n", [IPAddress, Port]).

tcp_listener_stoped(_Protocol,IPAddress,Port)->
	io:format("[~p:~p] listerner stopped~n", [IPAddress, Port]).

active_listeners()->
	[].
node_listeners(_Node) ->
	[].
on_node_down(_Node) ->
    ok.

start_client(Sock, SockTransform) ->
	%% start client_sup and make client_gen_server
    {ok, _Child, Reader} = supervisor:start_child(tcp_client_sup, []),
    ok = finch_net:controlling_process(Sock, Reader),
	%% start recv
    Reader ! {go, Sock, SockTransform},

    %% In the event that somebody floods us with connections, the
    %% reader processes can spew log events at error_logger faster
    %% than it can keep up, causing its mailbox to grow unbounded
    %% until we eat all the memory available and crash. So here is a
    %% meaningless synchronous call to the underlying gen_event
    %% mechanism. When it returns the mailbox is drained, and we
    %% return to our caller to accept more connetions.
    gen_event:which_handlers(error_logger),

    Reader.

start_client(Sock) ->
    start_client(Sock, fun (S) -> {ok, S} end).

%%--------------------------------------------------------------------
tcp_opts() ->
	case application:get_env(finch_server,tcp_listen_options) of
		undefined -> ?TCP_OPTIONS;
	    {ok, Opts}-> Opts
	end.

%%--------------------------------------------------------------------

%% There are three kinds of machine (for our purposes).
%%
%% * Those which treat IPv4 addresses as a special kind of IPv6 address
%%   ("Single stack")
%%   - Linux by default, Windows Vista and later
%%   - We also treat any (hypothetical?) IPv6-only machine the same way
%% * Those which consider IPv6 and IPv4 to be completely separate things
%%   ("Dual stack")
%%   - OpenBSD, Windows XP / 2003, Linux if so configured
%% * Those which do not support IPv6.
%%   - Ancient/weird OSes, Linux if so configured
%%
%% How to reconfigure Linux to test this:
%% Single stack (default):
%% echo 0 > /proc/sys/net/ipv6/bindv6only
%% Dual stack:
%% echo 1 > /proc/sys/net/ipv6/bindv6only
%% IPv4 only:
%% add ipv6.disable=1 to GRUB_CMDLINE_LINUX_DEFAULT in /etc/default/grub then
%% sudo update-grub && sudo reboot
%%
%% This matters in (and only in) the case where the sysadmin (or the
%% app descriptor) has only supplied a port and we wish to bind to
%% "all addresses". This means different things depending on whether
%% we're single or dual stack. On single stack binding to "::"
%% implicitly includes all IPv4 addresses, and subsequently attempting
%% to bind to "0.0.0.0" will fail. On dual stack, binding to "::" will
%% only bind to IPv6 addresses, and we need another listener bound to
%% "0.0.0.0" for IPv4. Finally, on IPv4-only systems we of course only
%% want to bind to "0.0.0.0".
%%
%% Unfortunately it seems there is no way to detect single vs dual stack
%% apart from attempting to bind to the port.
port_to_listeners(Port) ->
    IPv4 = {"0.0.0.0", Port, inet},
    IPv6 = {"::",      Port, inet6},
    case ipv6_status(10000) of
        single_stack -> [IPv6];
        ipv6_only    -> [IPv6];
        dual_stack   -> [IPv6, IPv4];
        ipv4_only    -> [IPv4]
    end.

ipv6_status(TestPort) ->
    IPv4 = [inet,  {ip, {0,0,0,0}}],
    IPv6 = [inet6, {ip, {0,0,0,0,0,0,0,0}}],
    case gen_tcp:listen(TestPort, IPv6) of
        {ok, LSock6} ->
            case gen_tcp:listen(TestPort, IPv4) of
                {ok, LSock4} ->
                    %% Dual stack
                    gen_tcp:close(LSock6),
                    gen_tcp:close(LSock4),
                    dual_stack;
                %% Checking the error here would only let us
                %% distinguish single stack IPv6 / IPv4 vs IPv6 only,
                %% which we figure out below anyway.
                {error, _} ->
                    gen_tcp:close(LSock6),
                    case gen_tcp:listen(TestPort, IPv4) of
                        %% Single stack
                        {ok, LSock4}            -> gen_tcp:close(LSock4),
                                                   single_stack;
                        %% IPv6-only machine. Welcome to the future.
                        {error, eafnosupport}   -> ipv6_only; %% Linux
                        {error, eprotonosupport}-> ipv6_only; %% FreeBSD
                        %% Dual stack machine with something already
                        %% on IPv4.
                        {error, _}              -> ipv6_status(TestPort + 1)
                    end
            end;
        %% IPv4-only machine. Welcome to the 90s.
        {error, eafnosupport} -> %% Linux
            ipv4_only;
        {error, eprotonosupport} -> %% FreeBSD
            ipv4_only;
        %% Port in use
        {error, _} ->
            ipv6_status(TestPort + 1)
    end.

