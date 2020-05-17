-module(slave).

-export([handle_request/0]).

%% Handles requests from other processes, receiving a function, communicating back with its result
handle_request() ->
    receive {FromPid, Fun} -> FromPid ! Fun() end.
