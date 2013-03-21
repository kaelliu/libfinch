%%% File : common.hrl
%%% Description : common define here
%%% Author : kael liu
%%% Date : 2012-08-03 Friday
%%% may sperate it as different define file.
-define(MAX_WAIT,3000).

%%flash 843 policy
-define(FL_POLICY_REQ, <<"<pol">>).
%-define(FL_POLICY_REQ, <<"<policy-file-request/>\0">>).
-define(FL_POLICY_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).

%%tcp_listener params
% packet 0 is no extra bytes in every tcp package
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true},
					   {nodelay, false}, {delay_send, true}, {send_timeout, 5000},
					   {keepalive, true}, {exit_on_close, true}]).

%% db param
-define(DB, k_mysql_conn).
-define(DB_HOST, "localhost").
-define(DB_PORT, 3306).
-define(DB_USER, "root").
-define(DB_PASS, "2932615").
-define(DB_NAME, "kdb").
-define(DB_ENCODE, utf8).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS,        86400).

-define(DEFAULT_PORT,8888).