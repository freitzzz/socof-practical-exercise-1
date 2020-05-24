-module(work_distribution).

-export([load_test/0]).

register_receiver(Alias) ->
    case whereis(Alias) of
      undefined ->
	  register(Alias,
		   spawn(fun () -> receive_results_handler() end));
      _ -> ok
    end.

receive_results_handler() ->
    receive
      {result_compute_prime, ResultComputePrime} ->
	  io:format("Result Prime: ~w~n", [ResultComputePrime]),
	  receive_results_handler()
    end.

load_test() ->
    case whereis(m1) of
      undefined ->
	  master:new(m1),
	  m1 ! {work_receiver, {spawn_slaves, 20}},
	  register_receiver(work_receiver);
      _ -> ok
    end,
    Runs = lists:seq(1, 100),
    io:format("Starting load test...~n"),
    lists:foreach(fun (_) ->
			  RandomNumbers = [rand:uniform(1000)
					   || _ <- lists:seq(1, 1000)],
			  m1 ! {work_receiver, {compute_prime, RandomNumbers}}
		  end,
		  Runs),
    io:format("All load distributed.~n").
