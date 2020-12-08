%%%-------------------------------------------------------------------
%% @doc day_three public API
%% @end
%%%-------------------------------------------------------------------

-module(day_three_app).

-behaviour(application).

-export([start/2, stop/1, read_file/2, just_read_and_process_file/1, process_line/2, add_index/1]).

start(_StartType, _StartArgs) ->
    day_three_sup:start_link().

stop(_State) ->
    ok.

add_index(L) -> add_index(L, lists:seq(0, length(L) - 1), []).
add_index([], _, Res) -> Res;
add_index(_, [], Res) -> Res;
add_index([HEl|TEl], [HIdx|TIdx], Res) -> add_index(TEl, TIdx, [{HIdx, HEl}|Res]). 

move({XMove, YMove}, {XStart, YStart}, {XWrap, YEscape}, Map) ->
  if YStart + YMove >= YEscape ->
       escape;
     true ->
       NewPosition = {(XStart + XMove) rem XWrap, YStart + YMove},
       IsMember = lists:member(NewPosition, Map),
       if IsMember ->
            {tree, NewPosition};
          true ->
            {empty, NewPosition}
       end
  end.

move_until_escape(Move, Position, Size, Trees, Ps, Map) ->
  case move(Move, Position, Size, Map) of
    escape -> {Trees, Ps};
    {tree, NewPosition} -> 
      {X, Y} = NewPosition,
      NewP = {X, Y, tree},
      move_until_escape(Move, NewPosition, Size, Trees+1, [NewP|Ps], Map);
    {empty, NewPosition} -> 
      {X, Y} = NewPosition,
      NewP = {X, Y, empty},
      move_until_escape(Move, NewPosition, Size, Trees, [NewP|Ps], Map)
  end.

process_line(Pid, Res) ->
  case file:read_line(Pid) of
    {ok, Line} ->
      Y = length(Res),
      Trimmed = string:trim(Line),
      LocsInLine = lists:foldl(fun(Loc, Locs) ->
                                    case Loc of
                                      {Idx, 35} -> [{Idx, Y}|Locs];
                                      _ -> Locs
                                    end
                                end, 
                                [], 
                                add_index(Trimmed)),
      process_line(Pid, [LocsInLine|Res]);
    eof -> Res
  end.

length_of_first_line(Pid) ->
  case file:read_line(Pid) of
    {ok, Line} -> 
      file:position(Pid, 0),
      length(string:trim(Line));
    _ -> 
      io:fwrite("Couldn't read the first line~n"),
      0
  end.

read_file(Path, Move) ->
  case file:open(Path, read) of
    {ok, Pid} -> 
      XWrap = length_of_first_line(Pid),
      Unflattened = process_line(Pid, []),
      YEscape = length(Unflattened),
      io:fwrite("XWrap - ~B~n", [XWrap]),
      move_until_escape(Move, {0, 0}, {XWrap, YEscape}, 0, [{0, 0, empty}], lists:flatten(Unflattened));
    _ -> {error, "file not found, volečku"}
  end.


just_read_and_process_file(Path) ->
  case file:open(Path, read) of
    {ok, Pid} -> 
      lists:flatten(process_line(Pid, []));
    _ -> {error, "file not found, volečku"}
  end.

%% internal functions
