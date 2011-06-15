%%% @author David Iglesias <david.iglesias@udc.es>
%%% @doc
%%% Parses a file with traces to a format suitable for the
%%% "Visualizing-EUnit-tests" application
%%% @end
%%% Created :  6 Apr 2011 by David Iglesias

-module(parser).
-export([parse/1]).

%%--------------------------------------------------------------------
%% @doc Parses the file
%% @spec parse( (FileName::string()) | (FileContent::binary()) ) ->
%%             { Positive::[trace()], Negative::[trace()] }  |  error
%% @type trace() = [ trace_line() ]
%% @type trace_line() = { PackageName, FunctionName, Arguments }
%% @end
%%--------------------------------------------------------------------
parse(FileName) when is_list(FileName) ->
    case file:read_file(FileName) of
	{ok,Binary} ->
	    parse(Binary);
	{error,enoent} ->
	    io:format("File ~p does not exist~n", [FileName]),
	    error;
	{error,E} ->
	    io:format("Error: ~p~n", [E]),
	    error
    end;
parse(Binary) when is_binary(Binary) ->
    Traces = binary:split(Binary, <<"\n","\n">>, [global, trim]),
    parse(Traces,[],[]).


%%--------------------------------------------------------------------
%% @doc Internal parsing function. Includes two accumulator arguments
%%      for tail-recursivity
%% @end
%%--------------------------------------------------------------------
%% Pos and Neg are accumulators
parse([], Pos, Neg) ->
    {Pos, Neg};
parse([H|T], Pos, Neg) ->
    case parse_trace(H) of
	{positive, ParsedTrace} ->
	    parse(T, [ParsedTrace|Pos], Neg);
	{negative, ParsedTrace} ->
	    parse(T, Pos, [ParsedTrace|Neg]);
	{not_valid} ->
	    parse(T, Pos, Neg)
    end.

%%--------------------------------------------------------------------
%% @doc Parses a trace
%% @spec parse_trace(BinaryTrace::binary()) ->
%%                     {positive|negative, trace()} | {not_valid}
%% @end
%%--------------------------------------------------------------------
parse_trace(BinaryTrace) ->
    Lines = binary:split(BinaryTrace, <<"\n">>, [global, trim]),
    case Lines of
        [ <<"+">> | Body ] ->
	    {positive, parse_trace_body(Body)};
	[ <<"-">> | Body ] ->
	    {negative, parse_trace_body(Body)};
	_ ->
	    {not_valid}
    end.

%%--------------------------------------------------------------------
%% @doc Parses a trace body
%% @spec parse_trace_body(TraceBody::[binary()]) -> trace()
%% @end
%%--------------------------------------------------------------------
parse_trace_body(TraceBody) ->
    [parse_trace_line(L) || L <- TraceBody].
    
%%--------------------------------------------------------------------
%% @doc Parses a trace line
%% @spec parse_trace_line(TraceLine::binary()) -> trace_line()
%% @end
%%--------------------------------------------------------------------
parse_trace_line(TraceLine) ->
    [Class, Method] = binary:split(TraceLine, <<";;">>),
    { bin_to_atom(Class), bin_to_atom(Method), [] }.

%%--------------------------------------------------------------------
%% @doc Converts from binary to atom, stripping whitespaces
%% @spec bin_to_atom(binary()) -> atom()
%% @end
%%--------------------------------------------------------------------
bin_to_atom(Binary) ->
    list_to_atom(string:strip(binary:bin_to_list(Binary))).
