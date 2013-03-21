#!/bin/sh
# param comment
# emulator param is start with +
# +p:Sets the maximum number of concurrent processes for this system. 
# +K:Enables or disables the kernel poll functionality if the emulator supports it. Default is false (disabled)
# -smp:smp support,smp is Symmetrical Multi Processor,no smp is only one scheduler,with smp can have 1024
# -name:Makes the Erlang runtime system into a distributed node
# setcookie:cookie in node communicate
# -boot:Specifies the name of the boot file, File.boot, which is used to start the system. See init(3).
# -config:Specifies the name of a configuration file
# -s:Makes init call the specified function. Func defaults to start. 
# -extra:start as env of application,which can be get use init:get_plain_arguments()
erl +p 1024000 +K true -smp disable -name srv@127.0.0.1 setcookie finch_k -boot start_sasl -config log -s k_finch game_server_start -extra 127.0.0.1 8888 1