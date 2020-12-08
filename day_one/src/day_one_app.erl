%%%-------------------------------------------------------------------
%% @doc day_one public API
%% @end
%%%-------------------------------------------------------------------

-module(day_one_app).

-behaviour(application).

-export([start/2, stop/1, process_line/3, read_file/2, permute/3, process_list/2]).

start(_StartType, _StartArgs) ->
    day_one_sup:start_link().

stop(_State) ->
    ok.

% permute([], Size, [HList|TList]) -> permute([HList], Size, TList);

% This may never be invoked
% permute(Current, Size, List) when length(Current) == Size -> permute([], Size, List);

% These two exhaust the list if the size of Current is one less than Size
permute(_Current, _Size, []) -> [];
% If there is only one space left in current, fill All with HList|Current until List is nulled
permute(Current, Size, [HList|TList]) when Size - length(Current) == 1 ->
  [[HList|Current]|permute(Current, Size, TList)];

permute(Current, Size, [HList|TList]) ->
  permute(Current, Size, TList) ++ permute([HList|Current], Size, TList).

process_list([], _) -> {error, "Empty list of numbers"};
process_list(_, {Summers, _}) when Summers < 2 ->
  {error, "You are a pud"};
process_list(Numbers, {Summers, _}) when length(Numbers) < Summers ->
  {error, "The list of numbers diminished to nothing"};
process_list(Numbers, {Summers, ToHit}) ->
  case permute([], Summers, Numbers) of
    [] -> nada;
    GotIt -> 
      Mapy = lists:map(fun(Thurk) -> {Thurk, lists:sum(Thurk)} end, GotIt),
      Filtry = lists:filter(fun({_, Sum}) -> Sum == ToHit end, Mapy),
      Filtry
  end.

process_line(Pid, Numbers, Options) ->
  case file:read_line(Pid) of
    {ok, Line} ->
      Trimmed = string:trim(Line),
      case string:to_integer(Trimmed) of
        {error, _} -> process_line(Pid, Numbers, Options);
        {Int, _} -> process_line(Pid, [Int|Numbers], Options)
      end;
    eof -> process_list(Numbers, Options);
    {error, Reason} -> {error, Reason, Numbers}
  end.

read_file(Path, Options) ->
  case file:open(Path, read) of
    {ok, Pid} -> process_line(Pid, [], Options);
    _ -> {error, "file not found, vole"}
  end.
%% internal functions
