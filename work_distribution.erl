-module(work_distribution).

-export([compute_prime/1, is_prime/1, spawn_slaves/1]).

is_prime(N, N, 0) -> true;
is_prime(_, _, 0) -> false;
is_prime(N, A, _) -> is_prime(N, A + 1, (A + 1) rem N).

is_prime(0) -> false;
is_prime(1) -> false;
is_prime(N) -> is_prime(N, 1, 1).

compute_prime(Integers) -> Integers.

spawn_slaves(0) -> [];
spawn_slaves(N) ->
    spawn_slaves(N - 1) ++
      [spawn(fun work_distribution:compute_prime/1)].
