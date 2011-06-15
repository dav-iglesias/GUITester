%%%-------------------------------------------------------------------
%%% @author David Iglesias Fraga
%%% @doc
%%% Communicates Erlang modules and a Swing GUI
%%% @end
%%%-------------------------------------------------------------------
-module(swing_listener).

%% API
-export([start/0, stop/0]).

-define(SERVER_NAME, swing_listener).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> true
%% @end
%%--------------------------------------------------------------------
start() ->
    register(?SERVER_NAME, spawn(fun() -> receive_loop(initial_state()) end)).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    ?SERVER_NAME!{exit}.
%%%===================================================================
%%%===================================================================



initial_state() ->
    {undefined}.

receive_loop({Automata}) ->
    receive
	{Pid, {generate_fsm, TracesFile}} ->
	    io:format("Received: generate_fsm from ~p~n", [Pid]),
	    case parser:parse(TracesFile) of
		{Pos, Neg} ->
		    Pid!{self(), {ok, generate_fsm}},
		    receive_loop({bluefringe:qsm({Pos, Neg})});
		error ->
		    Pid!{self(), {error, generate_fsm}},
		    receive_loop({Automata})
	    end;
	{Pid, {view_fsm, DestFolder}} ->
	    io:format("Received: view_fsm from ~p~n", [Pid]),
	    case Automata of
		undefined ->
		    Pid!{self(), {error, view_fsm}},
		    receive_loop({Automata});
		_ ->
		    bluefringe_dot:visualize(Automata),
		    %% Move generated files to specified directory
 		    file:rename(eunit_fsm.dot, filename:join(DestFolder,"eunit_fsm.dot")),
		    file:rename(eunit_fsm.jpeg, filename:join(DestFolder,"eunit_fsm.jpeg")),
		    Pid!{self(), {ok, view_fsm}},
		    receive_loop({Automata})
	    end;
	{Pid, {generate_eqc, ModuleName, DestFolder}} ->
	    case Automata of
		undefined ->
		    Pid!{self(), {error, generate_eqc}},
		    receive_loop({Automata});
		_ ->
		    case eqc_generator:generate(Automata, list_to_atom(ModuleName), DestFolder) of
			error ->
			    Pid!{self(), {error, generate_eqc}};
			_ ->
			    Pid!{self(), {ok, generate_eqc}}
		    end,
		    receive_loop({Automata})
	    end;
	{Pid, Unknown} ->
	    io:format("Received unknown request: ~p~n", [Unknown]),
	    Pid!{self(), {error}},
	    receive_loop({Automata});
	{exit} ->
	    ok
    end.
