%% Synchronous term storage using module functions
%% Asynchronous term storage by communicating with process registered under tsp alias
%% Uses process dictionary as term storage
-module(term_storage).

-export([lookup/1, lookup/2, remove/1, start/0, stop/0,
	 store/2]).

start() ->
    case whereis(tsp) of
      undefined ->
	  register(tsp, spawn(fun () -> loop_operations() end));
      _ -> ok
    end.

%% Not working :(
stop() -> exit(whereis(tsp), normal).

store(Key, Value) ->
    operation_handler({store, Key, Value}, self()).

remove(Key) -> operation_handler({remove, Key}, self()).

lookup(Key) -> operation_handler({lookup, Key}, self()).

lookup(Key, FromPid) ->
    operation_handler({lookup, Key}, FromPid).

operation_handler(Operation, FromPid) ->
    tsp ! {FromPid, Operation},
    receive {tsp, Reply} -> Reply end.

loop_operations() ->
    receive
      {FromPid, {store, Key, Value}} ->
	  put({FromPid, Key}, Value),
	  FromPid ! {tsp, Value},
	  loop_operations();
      {FromPid, {remove, Key}} ->
	  erase({FromPid, Key}),
	  FromPid ! {tsp, ok},
	  loop_operations();
      {FromPid, {lookup, Key}} ->
	  FromPid ! {tsp, get({FromPid, Key})}, loop_operations()
    end.
