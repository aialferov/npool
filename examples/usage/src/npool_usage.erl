%%%-------------------------------------------------------------------
%%% Created: 26 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_usage).

-export([start/0, stop/0]).

-export([add/2, remove/1, workers/0]).
-export([call_request/2, cast_request/2, bad_request/1]).

-export([state/0, state_na/0]).

-define(Server, npool_usage_server).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

add(ID, State) -> npool_server:add(?Server, ID, State).
remove(ID) -> npool_server:remove(?Server, ID).

workers() -> [Worker || Worker = {_ID, Pid}
	<- npool_server:workers(?Server), is_pid(Pid)].

call_request(ID, Data) -> npool_server:call(?Server, ID, {call_request, Data}).
cast_request(ID, Data) -> npool_server:cast(?Server, ID, {cast_request, Data}).
bad_request(ID) -> npool_server:call(?Server, ID, bad_request).

state() -> {ok, "dynamic state"}.
state_na() -> {error, not_available}.
