%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 10 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_usage_server).
-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

start_link(ID, State, StartNotifyFun) ->
	gen_server:start_link(?MODULE, [ID, State, StartNotifyFun], []).

init([_ID, State, StartNotifyFun]) -> StartNotifyFun(self()), {ok, State}.

handle_cast({call, bad_request, _From}, _State) -> unknown:bad_request();

handle_cast({call, Request, From}, State) ->
	npool_server:reply(From,
		{io:format("call ~p ~p~n", [Request, State]), result}),
	{noreply, State};

handle_cast({cast, Request}, State) ->
	io:format("cast ~p ~p~n", [Request, State]),
	{noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_info(Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
