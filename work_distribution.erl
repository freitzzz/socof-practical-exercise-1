-module(work_distribution).

-export([load_test/0]).

load_test() ->
    master:new(m1),
    m1 ! {self(), {spawn_slaves, 5}},
    Runs = lists:seq(1, 1000),
    lists:foreach(fun (_) ->
			  io:format("Run~n"),
			  RandomNumbers = [rand:uniform(100)
					   || _ <- lists:seq(1, 1000)],
			  m1 ! {self(), {distribute_load, RandomNumbers}},
			  io:format("After send message~n")
		  end,
		  Runs).
