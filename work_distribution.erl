-module(work_distribution).

-export([handle_requests/0, link_slaves/2,
	 spawn_slaves/2]).

-import(prime_operations, [compute_prime/1]).

on_exit(Pid, Fun) ->
    spawn(fun () ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive {'EXIT', Pid, _} -> Fun(Pid) end
	  end).

handle_requests() ->
    receive
      {FromPid, Integers} ->
	  FromPid ! compute_prime(Integers), handle_requests()
    end.

spawn_slaves(0, _) -> [];
spawn_slaves(N, Fun) ->
    spawn_slaves(N - 1, Fun) ++ [spawn(Fun)].

link_slaves([], _) -> [];
link_slaves([H | T], Fun) ->
    [on_exit(H, Fun) | link_slaves(T, Fun)].
