-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win(GameState) ->
    case GameState of
      {{U, U, U}, {_, _, _}, {_, _, _}} when U /= f -> {win, U};
      {{_, _, _}, {U, U, U}, {_, _, _}} when U /= f -> {win, U};
      {{_, _, _}, {_, _, _}, {U, U, U}} when U /= f -> {win, U};
      {{_, _, U}, {_, _, U}, {_, _, U}} when U /= f -> {win, U};
      {{_, U, _}, {_, U, _}, {_, U, _}} when U /= f -> {win, U};
      {{U, _, _}, {U, _, _}, {U, _, _}} when U /= f -> {win, U};
      {{U, _, _}, {_, U, _}, {_, _, U}} when U /= f -> {win, U};
      {{_, _, U}, {_, U, _}, {U, _, _}} when U /= f -> {win, U};
      _ -> no_win
    end.


get_coords(Cell, Dim) ->
	Row = Cell div Dim,
	Column = Cell rem Dim,
	if
		Column == 0 -> {Row, Dim};
		true -> {Row+1, Column}
	end.


move(Cell, Player, GameState) ->
	Dim = 3,
  if
    Cell > Dim*Dim -> {error, invalid_move};
    (Player /= x) and (Player /= o) -> {error, invalid_player};
    true ->
      {RowIdx, ColIdx} = get_coords(Cell, Dim),
      CurrCellValue = element(ColIdx, element(RowIdx, GameState)),
      if
        CurrCellValue /= f -> {error, invalid_move};
        true ->
          Row = element(RowIdx, GameState),
          NewGameState = setelement(RowIdx, GameState, setelement(ColIdx, Row, Player)),
          {ok, NewGameState}
      end
  end.
