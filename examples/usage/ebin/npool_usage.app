%%%-------------------------------------------------------------------
%%% Created: 26 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, npool_usage, [
	{id, "npool_usage"},
	{vsn, "0.0.1"},
	{description, "NPool usage example"},
	{modules, [npool_usage, npool_usage_server]},
	{registered, [npool_usage_server]},
	{applications, [kernel, stdlib, sasl, npool]},
	{mod, {npool_app, [
		{module, npool_usage_server},
		{worker_spec, {1, 1, temporary, infinity}}
	]}}
]}.
