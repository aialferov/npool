%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 10 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) -> supervisor:start_link(?MODULE, Args).

init(Args) -> {ok, {{one_for_all, 1, 10}, [
	{npool, {npool_server, start_link, Args ++ [self()]},
	permanent, infinity, worker, [npool_server]}
]}}.
