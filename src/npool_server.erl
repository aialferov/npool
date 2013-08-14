%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 10 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(npool_server).
-behaviour(gen_server).

-export([start_link/2]).

-export([add/3, remove/2]).
-export([workers/1]).

-export([call/3, call/4, cast/3]).
-export([reply/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(WorkerSpec(Module), {
	Module, {npool_worker_sup, start_link, [Module]},
	permanent, infinity, supervisor, [npool_worker_sup]
}).

start_link({module, Module}, SupPid) -> gen_server:start_link({local, Module},
	?MODULE, {Module, utils_app:get_env([states]), SupPid}, []).

add(Name, ID, State) -> gen_server:call(Name, {add, ID, State}).
remove(Name, ID) -> gen_server:call(Name, {remove, ID}).

workers(Name) -> [Worker || Worker = {_ID, Pid}
	<- gen_server:call(Name, workers), is_pid(Pid)].

call(Name, ID, Request) -> gen_server:call(Name, {call, ID, Request}).
call(Name, ID, Request, Timeout) ->
	gen_server:call(Name, {call, ID, Request}, Timeout).

cast(Name, ID, Request) -> gen_server:call(Name, {cast, ID, Request}).

reply(Client, Reply) -> gen_server:reply(Client, Reply).

init({Module, [], SupPid}) -> init({Module, [{states, []}], SupPid});
init({Module, [{states, States}], SupPid}) ->
	{gen_server:cast(Module, {start_workers, Module, States, SupPid}), []}.

handle_call({add, ID, WorkerState}, _From, State = {WorkerSupPid, Workers}) ->
	case npool_workers:member(ID, Workers) of
		true -> {reply, {error, already_started}, State};
		false -> case start_worker(WorkerSupPid, {ID, WorkerState}) of
			{ok, WorkerPid} -> {reply, ok, {WorkerSupPid,
				npool_workers:insert({ID, WorkerPid}, Workers)}};
			Error -> {reply, Error, State}
		end
	end;

handle_call({remove, ID}, _From, State = {WorkerSupPid, Workers}) ->
	case worker_pid(ID, Workers) of
		{ok, WorkerPid} ->
			{reply, supervisor:terminate_child(WorkerSupPid, WorkerPid),
				{WorkerSupPid, npool_workers:delete(ID, Workers)}};
		Error -> {reply, Error, State}
	end;

handle_call(workers, _From, State = {_WorkerSupPid, Workers}) ->
	{reply, npool_workers:all(Workers), State};

handle_call({call, ID, Request}, From, State = {_WorkerSupPid, Workers}) ->
	case worker_pid(ID, Workers) of
		{ok, WorkerPid} ->
			gen_server:cast(WorkerPid, {call, Request, From}),
			{noreply, State};
		Error -> {reply, Error, State}
	end;

handle_call({cast, ID, Request}, _From, State = {_WorkerSupPid, Workers}) ->
	{reply, case worker_pid(ID, Workers) of
		{ok, WorkerPid} -> gen_server:cast(WorkerPid, {cast, Request}); 
		Error -> Error
	end, State}.

handle_cast({start_workers, Module, States, SupPid}, []) ->
	{noreply, start_workers(start_worker_sup(Module, SupPid), States)};

handle_cast({worker_started, ID, WorkerPid}, {WorkerSupPid, Workers}) ->
	{noreply, {WorkerSupPid, case npool_workers:member(ID, Workers) of
		true -> Workers;
		false ->
			monitor(process, WorkerPid),
			npool_workers:insert({ID, WorkerPid}, Workers)
	end}}.

handle_info(
	{'DOWN', _MonitorRef, process, WorkerPid, _Info},
	{WorkerSupPid, Workers}
) ->
	{noreply, {WorkerSupPid, npool_workers:delete(WorkerPid, Workers)}}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


worker_pid(ID, Workers) -> worker_pid(npool_workers:lookup(ID, Workers)).
worker_pid({_ID, WorkerPid}) -> case is_process_alive(WorkerPid) of
	true -> {ok, WorkerPid}; false -> {error, internal} end;
worker_pid(false) -> {error, not_found}.

start_worker_sup(Module, SupPid) ->
	case supervisor:start_child(SupPid, ?WorkerSpec(Module)) of
		{ok, WorkerSupPid} -> WorkerSupPid;
		{error, {already_started, WorkerSupPid}} -> WorkerSupPid
	end.

start_workers(WorkerSupPid, States) -> {WorkerSupPid, npool_workers:init(
	[start_worker(WorkerSupPid, worker_state(State)) || State <- States])}.

start_worker(_WorkerSupPid, []) -> [];
start_worker(WorkerSupPid, {ID, State}) -> start_worker(supervisor:start_child(
	WorkerSupPid, [ID, State, worker_start_notify_fun(self(), ID)])).

start_worker(Ok = {ok, WorkerPid}) -> monitor(process, WorkerPid), Ok;
start_worker(Error) -> Error.

worker_state({static, {ID, State}}) -> {ID, State};
worker_state({dynamic, {ID, {M, F}}}) -> case M:F() of
	{ok, State} -> {ID, State};
	{error, not_available} -> []
end.

worker_start_notify_fun(ServerPid, ID) -> fun(WorkerPid) ->
	gen_server:cast(ServerPid, {worker_started, ID, WorkerPid}) end.
