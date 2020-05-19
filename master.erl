-module(master).

-export([new/1]).

new(Master) ->
    register(Master,
	     spawn(fun () ->
			   term_storage:start(), loop_operations(Master)
		   end)).

loop_operations(Master) ->
    receive
      {FromPid, {spawn_slaves, N}} ->
	  spawn_new_slaves(N),
	  FromPid ! {Master, ok},
	  loop_operations(Master);
      {FromPid, {distribute_load, Integers}} ->
	  AvailableSlaves = term_storage:lookup(slaves),
	  distribute_load(Master, Integers, AvailableSlaves),
	  FromPid ! {Master, ok},
	  loop_operations(Master);
      {FromPid, {result_compute_prime, Result}} ->
	  io:format("Result Prime Computation: ~w~n", [Result]),
	  FromPid ! {Master, ok},
	  loop_operations(Master)
    end.

spawn_new_slaves(N) ->
    OnExitFun = fun (SPid) -> term_storage:remove(SPid) end,
    Slaves = spawn_slaves(N),
    store_slaves(Slaves),
    link_on_exit_slaves(Slaves, OnExitFun).

spawn_slaves(0) -> [];
spawn_slaves(N) ->
    spawn_slaves(N - 1) ++
      [spawn(fun slave:handle_request/0)].

store_slaves(Slaves) ->
    term_storage:store(slaves, Slaves).

link_on_exit_slaves([], _) -> [];
link_on_exit_slaves([H | T], Fun) ->
    [on_exit(H, Fun) | link_on_exit_slaves(T, Fun)].

on_exit(Pid, Fun) ->
    spawn(fun () ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive
		    {'EXIT', Pid, _} -> io:format("Dead ~n"), Fun(Pid)
		  end
	  end).

distribute_load(Master, Integers, Slaves) ->
    Parcels = split_integers_in_parcels(Integers),
    distribute_parcels(Master, Parcels, Slaves).

distribute_parcels(_, [], _) -> [];
distribute_parcels(Master, [HP | TP], [HS | TS]) ->
    HS ! {Master, {compute_prime, HP}},
    distribute_parcels(Master, TP, TS).

% split_integers_in_parcels(_, 0) -> [];
% split_integers_in_parcels(Integers, 1) -> [Integers];
% split_integers_in_parcels(Integers, 2) ->
%     {L1, L2} = lists:split(2, Integers), [L1, L2];
% split_integers_in_parcels(Integers, 3) ->
%     {L1, L2} = lists:split(2, Integers),
%     {L11, L12} = lists:split(1, L1),
%     [L11, L12, L2];
% split_integers_in_parcels(Integers, NParcels) ->
%     A = round(length(Integers) / 2),
%     {L1, L2} = lists:split(A, Integers),
%     split_integers_in_parcels(L1, NParcels - 2) ++
%       split_integers_in_parcels(L2, NParcels - 2).

split_integers_in_parcels(Integers) ->
    case length(Integers) =< 10 of
      true -> [Integers];
      false ->
	  A = round(length(Integers) / 2),
	  {L1, L2} = lists:split(A, Integers),
	  split_integers_in_parcels(L1) ++
	    split_integers_in_parcels(L2)
    end.
