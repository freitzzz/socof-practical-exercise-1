-module(work_distribution).

-export([load_test/0]).

load_test() ->
    case whereis(m1) of
      undefined -> master:new(m1);
      _ -> ok
    end,
    m1 ! {self(), {spawn_slaves, 20}},
    Runs = lists:seq(1, 100),
    lists:foreach(fun (_) ->
			  io:format("Run~n"),
			  RandomNumbers = [rand:uniform(100)
					   || _ <- lists:seq(1, 1000)],
			  m1 ! {self(), {compute_prime, RandomNumbers}},
			  io:format("After send message~n")
		  end,
		  Runs).
