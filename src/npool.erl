%%%-------------------------------------------------------------------
%%% Created: 26 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool).
-export([start/0, stop/0]).

start() -> application:start(npool).
stop() -> application:stop(npool).
