-module(erlias).
-export([build/2]).
-include_lib("syntax_tools/include/merl.hrl").

build(BaseMod, Alias) ->
	msg(io_lib:format("Building alias ~p ==> ~p.",[Alias, BaseMod])),
	Exports = BaseMod:module_info(exports),
	Modtext = build_module(BaseMod, Alias, Exports),
	Forms = merl:quote(Modtext),
	Res = merl:compile_and_load(Forms),
	case Res of 
		{ok, _} -> ok;
		error ->
			error_logger:error_msg("Erlias: Unable to compile alias module (~p).~n~s~n", [Modtext]),
			error
	end.

build_module(BaseMod, Alias, Exports) ->
	lists:flatten([
		build_header(Alias),
		build_exports(BaseMod, Exports)
	]).

build_header(Alias) ->
	"-module(" ++ atom_to_list(Alias) ++ ").\n"
	"-compile(export_all).\n".

build_exports(BaseMod, Exports) ->
	[build_export(BaseMod, FunArity) || FunArity <- Exports].

build_export(_, {module_info,_}) ->
	""; %% ignore module_info
build_export(BaseMod, {Fun, Arity}) ->
	Arglist = arglist(Arity),
	StrFun = atom_to_list(Fun),
	FunCall = StrFun ++ "(" ++ Arglist ++ ")",
	_Line = prefixize(BaseMod, FunCall).

prefixize(BaseMod, FunCall) ->
	FunCall ++ " -> " ++ atom_to_list(BaseMod) ++ ":" ++ FunCall ++ ".\n".
	
arglist( Argc) ->
	Args = lists:seq($A, $A+Argc-1),
	StrArgs = [[Arg] || Arg <- Args],
	string:join(StrArgs, ",").

msg(Msg) ->
	io:format("Erlias: ~s~n",[Msg]).
