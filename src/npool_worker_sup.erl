%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 10 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_worker_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(Module, WorkerSpec) ->
	supervisor:start_link(?MODULE, {Module, WorkerSpec}).

init({Module, {MaxR, MaxT, Restart, Shutdown}}) ->
	{ok, {{simple_one_for_one, MaxR, MaxT}, [
		{npool_worker_sup, {Module, start_link, []},
		Restart, Shutdown, worker, [Module]}
	]}}.
