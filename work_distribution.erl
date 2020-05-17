-module(work_distribution).

-export([compute_prime/1, find_prime_integers/1,
	 handle_requests/0, is_prime/1, link_slaves/2,
	 spawn_slaves/2]).

is_prime(N, N, 0) -> true;
is_prime(_, _, 0) -> false;
is_prime(N, A, _) -> is_prime(N, A + 1, N rem (A + 1)).

is_prime(0) -> false;
is_prime(1) -> false;
is_prime(N) -> is_prime(N, 1, 1).

find_prime_integers([]) -> [];
find_prime_integers([H | T]) ->
    case is_prime(H) of
      true -> [H | find_prime_integers(T)];
      false -> find_prime_integers(T)
    end.

on_exit(Pid, Fun) ->
    spawn(fun () ->
		  process_flag(trap_exit, true),
		  link(Pid),
		  receive {'EXIT', Pid, _} -> Fun(Pid) end
	  end).

compute_prime(Integers) ->
    lists:foldl(fun (X, Sum) -> X + Sum end, 0,
		find_prime_integers(Integers)).

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
