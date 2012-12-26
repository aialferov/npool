%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 10 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_worker_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Module) -> supervisor:start_link(?MODULE, [Module]).

init([Module]) -> {ok, {{simple_one_for_one, 1, 10}, [
	{npool_worker_sup, {Module, start_link, []},
	permanent, infinity, worker, [Module]}
]}}.
