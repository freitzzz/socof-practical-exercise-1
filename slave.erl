-module(slave).

-export([handle_request/0]).

handle_request() ->
    receive
      {FromPid, {compute_prime, Integers}} ->
	  Result = prime_operations:compute_prime(Integers),
	  FromPid ! {self(), {result_compute_prime, Result}},
	  handle_request()
    end.
