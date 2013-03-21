-define(CMD_BAD, 2560).
-define(CMD_LOGIN,1).
%% -record(x,{d1,d2,d3}).
%% x-tag,d1-d3:data field
%% buffer: 2byte(length of buffer)+2byte(command)+other/binary(data,encrypted)
%% 4 BYTES before
-record(bad,{cmd,error}).
-record(login,{usr_name,passwd}).

