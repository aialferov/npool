%%%-------------------------------------------------------------------
%%% Created: 26 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, npool_usage, [
	{id, "npool_usage"},
	{vsn, "0.0.1"},
	{description, "NPool usage example"},
	{modules, [
		npool_usage,
		npool_usage_server
	]},
	{registered, [npool_usage_server]},
	{applications, [kernel, stdlib, sasl, utils]},
	{mod, {npool_app, [npool_usage_server]}}
]}.
