-module(state_provider).
-export([state1/0, state2/0]).

state1() -> {ok, "dynamic state 1"}.
state2() -> {error, not_available}.
