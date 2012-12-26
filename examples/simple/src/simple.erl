%%%-------------------------------------------------------------------
%%% Created: 26 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(simple).
-export([start/0, stop/0]).

start() -> application:start(simple).
stop() -> application:stop(simple).
