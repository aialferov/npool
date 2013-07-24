%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 25 Jul 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_workers).

-export([init/1]).

-export([member/2, lookup/2]).
-export([insert/2, delete/2]).

-define(Options, [set, protected, named_table]).

init(Workers) -> init(Workers, ets:new(
	list_to_atom(erlang:ref_to_list(make_ref())), ?Options)).
init(Workers, Tid) -> lists:foreach(fun([]) -> ok;
	(Worker) -> insert(Worker, Tid) end, Workers), Tid.

member(Key, Tid) -> ets:member(Tid, Key).

lookup(Key, Tid) -> case ets:lookup(Tid, Key) of
	[] -> false; [Result] -> Result end.

insert({ID, Pid}, Tid) ->
	ets:insert(Tid, {ID, Pid}),
	ets:insert(Tid, {Pid, ID}), Tid.

delete(Key, Tid) -> delete_pair(Tid, lookup(Key, Tid)).

delete_pair(Tid, {ID, Pid}) ->
	ets:delete(Tid, ID),
	ets:delete(Tid, Pid), Tid;
delete_pair(Tid, false) -> Tid.
