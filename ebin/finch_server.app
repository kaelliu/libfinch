%% auther: kael liu   
%% email: ikaelliu@gmail.com   
%% date: 2013.03.20

{   
    application, finch_server,
    [   
        {description, "K game server."},   
        {vsn, "0.1"},   
        {modules,[]},   
        {registered, [finch_sup]},   
        {applications, [kernel, stdlib, sasl]},   
        {mod, {finch_server_app, []}},   
        {start_phases, []}   
    ]   
}.    

%% File end. 