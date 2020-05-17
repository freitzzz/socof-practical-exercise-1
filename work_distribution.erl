-module(work_distribution).

-export([compute_prime/1, spawn_slaves/1]).

compute_prime(Integers) -> Integers.

spawn_slaves(0) -> [];
spawn_slaves(N) ->
    spawn_slaves(N - 1) ++
      [spawn(fun work_distribution:compute_prime/1)].
