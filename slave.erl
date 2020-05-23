-module(slave).

-export([handle_request/0]).

handle_request() ->
    receive
      {FromPid, {compute_prime_slave, Integers, UUID}} ->
	  Result = prime_operations:compute_prime(Integers),
	  FromPid !
	    {self(), {result_compute_prime_slave, Result, UUID}},
	  handle_request()
    end.
