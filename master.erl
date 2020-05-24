-module(master).

-export([new/1]).

new(Master) ->
    register(Master,
	     spawn(fun () ->
			   term_storage:start(), loop_operations(Master)
		   end)).

%% Fault error controls missing:
%% - If slave dies before sending the result message of the prime compute, the result check will be inconsistent
%% - Term dictionary is probably not concurrent-safe? So multiple read-write might cause inconsistencies
%% - modules are not linked to each other, causes badarg if dependant modules are not compiled previously
%% Needs adjustements on split integers list in parcels algorithm

loop_operations(Master) ->
    receive
      {FromPid, {spawn_slaves, N}} ->
	  spawn_new_slaves(N),
	  FromPid ! {Master, ok},
	  loop_operations(Master);
      {FromPid, {compute_prime, Integers}} ->
	  Code = distribute_load_compute_prime(Master, Integers,
					       FromPid),
	  FromPid ! {Master, Code},
	  loop_operations(Master);
      {FromPid, {result_compute_prime_slave, Result, UUID}} ->
	  on_result_compute_prime_slave(Result, UUID),
	  FromPid ! {Master, ok},
	  loop_operations(Master)
    end.

spawn_new_slaves(N) ->
    MPid = self(),
    OnExitFun = fun (SPid) ->
			Slaves = term_storage:lookup(slaves, MPid),
			UpdatedSlaves = lists:delete(SPid, Slaves),
			term_storage:store(slaves, UpdatedSlaves)
		end,
    Slaves = spawn_slaves(N),
    store_slaves(Slaves),
    link_on_exit_slaves(Slaves, OnExitFun).

spawn_slaves(0) -> [];
spawn_slaves(N) ->
    spawn_slaves(N - 1) ++
      [spawn(fun slave:handle_request/0)].

store_slaves(Slaves) ->
    ExistingSlaves = term_storage:lookup(slaves),
    case ExistingSlaves of
      undefined -> term_storage:store(slaves, Slaves);
      _ ->
	  UpdatedSlaves = Slaves ++ ExistingSlaves,
	  term_storage:store(slaves, UpdatedSlaves)
    end.

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

on_result_compute_prime_slave(Result, UUID) ->
    {ParcelsLength, ResultList, FromPid} =
	term_storage:lookup(UUID),
    NewResultList = [Result] ++ ResultList,
    case ParcelsLength == length(NewResultList) of
      true ->
	  ResultComputePrime = lists:sum(NewResultList),
	  term_storage:remove(UUID),
	  FromPid ! {result_compute_prime, ResultComputePrime};
      false ->
	  term_storage:store(UUID,
			     {ParcelsLength, NewResultList, FromPid})
    end.

distribute_load_compute_prime(Master, Integers,
			      FromPid) ->
    AvailableSlaves = term_storage:lookup(slaves),
    Parcels = distribute_load_in_parcels(Integers,
					 AvailableSlaves),
    UUID = erlang:timestamp(),
    term_storage:store(UUID,
		       {length(Parcels), [], FromPid}),
    distribute_parcels(Master, Parcels, AvailableSlaves,
		       UUID),
    ok.

distribute_load_in_parcels(Integers, Slaves) ->
    ParcelFactor = round(length(Integers) / length(Slaves))
		     + 1,
    split_integers_in_parcels(Integers, ParcelFactor).

distribute_parcels(_, [], _, _) -> [];
distribute_parcels(Master, [HP | TP], [HS | TS],
		   UUID) ->
    HS ! {Master, {compute_prime_slave, HP, UUID}},
    distribute_parcels(Master, TP, TS, UUID).

split_integers_in_parcels(Integers, ParcelFactor) ->
    case length(Integers) =< ParcelFactor * 1.5 of
      true -> [Integers];
      false ->
	  A = round(length(Integers) / 2),
	  {L1, L2} = lists:split(A, Integers),
	  split_integers_in_parcels(L1, ParcelFactor) ++
	    split_integers_in_parcels(L2, ParcelFactor)
    end.
