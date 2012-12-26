%%%-------------------------------------------------------------------
%%% Created: 10 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, npool, [
	{id, "npool"},
	{vsn, "0.0.1"},
	{description, "The pool of processes"},
	{modules, [
		npool,
		npool_app,
		npool_sup,
		npool_server,
		npool_worker_sup
	]},
	{registered, []},
	{applications, [kernel, stdlib, sasl]}
]}.
