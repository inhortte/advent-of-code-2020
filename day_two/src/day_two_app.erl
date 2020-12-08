%%%-------------------------------------------------------------------
%% @doc day_two public API
%% @end
%%%-------------------------------------------------------------------

-module(day_two_app).

-behaviour(application).

-export([start/2, stop/1, parse/1, validate_part_one/1, validate_part_two/1, process_line/2, read_file/1]).

start(_StartType, _StartArgs) ->
    day_two_sup:start_link().

stop(_State) ->
    ok.

parse(Line) ->
  case lists:map(fun(S) -> string:trim(S) end, string:split(Line, ":")) of
    [Policy, Password] -> 
      case lists:map(fun(S) -> string:trim(S) end, string:split(Policy, " ")) of
        [Termini, Char] -> 
          case lists:map(fun(S) -> string:trim(S) end, string:split(Termini, "-")) of
            [StartString, EndString] ->
              case string:to_integer(StartString) of
                {error, _} -> {error, "start must be an integer"};
                {Start, _} ->
                  case string:to_integer(EndString) of
                    {error, _} -> {error, "end must be an integer"};
                    {End, _} -> {Start, End, Char, Password}
                  end
              end;
            _ -> {error, "garbled termini"}
          end;
        _ -> {error, "garbled policy / character"}
      end;
    _ -> {error, "garbled line"}
  end. 

validate_part_one(Line) ->
  case parse(Line) of
    {error, Error} -> {error, Error};
    {Start, End, Char, Password} ->
      Pertinent = length(lists:filter(fun(C) -> C == hd(Char) end, Password)),
      (Pertinent >= Start) and (Pertinent =< End)
  end.

validate_part_two(Line) ->
  case parse(Line) of
    {error, Error} -> {error, Error};
    {Start, End, Char, Password} ->
      if (Start < 1) or (Start > length(Password)) or (End > length(Password)) ->
           false;
         true ->
           StartChar = hd(string:slice(Password, Start - 1, 1)) =:= hd(Char),
           EndChar = hd(string:slice(Password, End - 1, 1)) =:= hd(Char),
           StartChar xor EndChar
      end
  end.

process_line(Pid, Res) ->
  case file:read_line(Pid) of
    {ok, Line} ->
      Trimmed = string:trim(Line),
      case validate_part_two(Trimmed) of
        {error, _} -> process_line(Pid, [{Trimmed, malformed}|Res]);
        GotIt -> process_line(Pid, [{Trimmed, GotIt}|Res])
      end;
    eof -> Res
  end.

read_file(Path) ->
  case file:open(Path, read) of
    {ok, Pid} -> process_line(Pid, []);
    _ -> {error, "file not found, vole"}
  end.
