%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 10 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) -> npool_sup:start_link(StartArgs).
stop(_State) -> ok.
