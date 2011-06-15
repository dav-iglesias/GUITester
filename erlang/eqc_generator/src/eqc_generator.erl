%%% @author David Iglesias <david.iglesias@udc.es>
%%% @doc
%%% Generates a module with the callbacks for the eqc_fsm
%%% according to an automata received
%%% @end
%%% Created :  12 Apr 2011 by David Iglesias

-module(eqc_generator).
-export([generate/3]).
-include("../../Visualizing-EUnit-tests/include/automata.hrl").
%-include("/home/david/pfc/repo/PFC-DIglesias/iu/trazas/Visualizing-EUnit-tests/include/automata.hrl").
-define(TEMPLATE_FILE, "eqc_generator/priv/skeleton_eqc_fsm.dat").

generate(Automata, ModuleName, DestFolder) ->
    SubstitutionList = get_substitutions(Automata, ModuleName),
    generate_from_template(?TEMPLATE_FILE, ModuleName, SubstitutionList, DestFolder).

get_substitutions(#fa{st=States, iSt=InitialState,
			   tr=Transitions, fSt=FinalStates}, ModuleName) ->
    S1 = add_substitution([], "HEADER", header()),
    S2 = add_substitution(S1, "MODULE_NAME", module_name(ModuleName)),
    S3 = add_substitution(S2, "STATES_DEFINITION", states_definition(States, Transitions)),
    S4 = add_substitution(S3, "INITIAL_STATE", initial_state(InitialState)),
    add_substitution(S4, "POSTCONDITIONS", postconditions(FinalStates)).

add_substitution(SubstitutionList, Token, Content) ->
    [{subst, Token, Content} | SubstitutionList].

%% BEGIN content generators
%% They must return a fun(IoDevice) -> ok

header() ->
    fun(IoDevice) ->
	    io:format(IoDevice, "%%% Autogenerated by the tool.~n", []),
	    io:format(IoDevice, "%%% Modify the property and weight function if necessary~n", []),
	    io:format(IoDevice, "%%% Feel free to modify the rest of the file~n", [])
    end.

module_name(Name) ->
    fun(IoDevice) ->
	    io:format(IoDevice, "-module(~p).~n", [Name])
    end.

initial_state(State) ->
    fun(IoDevice) ->
	    io:format(IoDevice, "    state~p.~n", [State])
    end.

postconditions(FinalStates) ->
    fun(IoDevice) ->
	    generate_postconditions(FinalStates, IoDevice)
    end.

generate_postconditions([], _IoDevice) ->
    ok;
generate_postconditions([H|T], IoDevice) ->
    io:format(IoDevice, "postcondition(_From,state~p,_S,{call,_,_,_},_Res) ->~n", [H]),
    io:format(IoDevice, "    false;~n", []),
    generate_postconditions(T, IoDevice).

states_definition(States, Transitions) ->
    fun(IoDevice) ->
	    SortedTransitions = lists:keysort(1, Transitions),
	    SortedStates = lists:sort(States),
	    generate_states(SortedStates, SortedTransitions, IoDevice)
    end.

generate_states([], _Transitions, _IoDevice) ->
    ok;
generate_states([State|T], Transitions, IoDevice) ->
    generate_state_header(State, IoDevice),
    RestOfTransitions = generate_transitions(Transitions, State, IoDevice, true),
    generate_states(T, RestOfTransitions, IoDevice).

generate_state_header(State, IoDevice) ->
    io:format(IoDevice, "state~p(_S) ->~n", [State]),
    io:format(IoDevice, "    [~n", []).

generate_transitions([], _State, IoDevice, _FirstTransition) ->
    io:format(IoDevice, "~n    ].~n", []),
    [];
generate_transitions([{From,{Pckg,Fun,Args},To} | T], State, IoDevice, true) when From==State ->
    io:format(IoDevice, "     {state~p, {call, ~p, ~p, ~p}}", [To,Pckg,Fun,Args]),
    generate_transitions(T, State, IoDevice, false);
generate_transitions([{From,{Pckg,Fun,Args},To} | T], State, IoDevice, false) when From==State ->
    io:format(IoDevice, ",~n     {state~p, {call, ~p, ~p, ~p}}", [To,Pckg,Fun,Args]),
    generate_transitions(T, State, IoDevice, false);
generate_transitions(Transitions, _State, IoDevice, _FirstTransition) ->
    io:format(IoDevice, "~n    ].~n", []),
    Transitions.

%% END content generators

generate_from_template(TemplateFile, ModuleName, SubstitutionList, DestFolder) ->
    case file:open(TemplateFile, [read]) of
	{error, Reason} ->
	    io:format("Error reading template file: ~p~n", [Reason]),
	    error;
	{ok, InIoDevice} ->
	    case file:open(filename:join(DestFolder, atom_to_list(ModuleName)++".erl"), [write]) of
		{error, Reason} ->
		    io:format("Error opening output file: ~p~n", [Reason]),
		    error;
		{ok, OutIoDevice} ->
		    copy(InIoDevice, OutIoDevice, SubstitutionList)
	    end
    end.
    
copy(InIoDevice, OutIoDevice, SubstitutionList) ->
    % Skip first line (it is a warning to the user)
    file:read_line(InIoDevice),
    read_loop(InIoDevice, OutIoDevice, SubstitutionList).

read_loop(InIoDevice, OutIoDevice, SubstitutionList) ->
    case file:read_line(InIoDevice) of
	{ok, Line} ->
	    subst_and_write_line(Line, SubstitutionList, OutIoDevice),
	    read_loop(InIoDevice, OutIoDevice, SubstitutionList);
	eof ->
	    file:close(OutIoDevice),
	    file:close(InIoDevice),
	    ok;
	{error, Reason} ->
	    io:format("Error reading template file: ~p~n", [Reason]),
	    error
    end.

subst_and_write_line([$_,$_|Token], SubstitutionList, IoDevice) ->
    CleanToken = string:strip(Token, right, $\n),
    case lists:keyfind(CleanToken, 2, SubstitutionList) of
	{subst, CleanToken, ContentGenerator} ->
	    ContentGenerator(IoDevice);
	    %file:write(IoDevice, Content);
	false ->
	    io:format("Cannot find substitution for ~p~n", [CleanToken]),
	    error
    end;
subst_and_write_line(NormalLine, _SubstitutionList, IoDevice) ->
    file:write(IoDevice, NormalLine).

    
