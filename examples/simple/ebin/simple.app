%%%-------------------------------------------------------------------
%%% Created: 26 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, simple, [
	{id, "simple"},
	{vsn, "0.0.1"},
	{description, "Simple example of the NPool app"},
	{modules, [
		simple,
		simple_server
	]},
	{registered, [simple_server]},
	{applications, [kernel, stdlib, sasl, npool]},
	{mod, {npool_app, []}}
]}.
