-module(lesson06).

-export([init/0, count_record/1, dest_table_init/0, move/2]).

-include_lib("stdlib/include/ms_transform.hrl").

%%-record(user, {
%%  name,
%%  age,
%%  gender
%%}).
%%
%%
%%get_users() ->
%%		[
%%			#user{name="Bob", age=29, gender=male},
%%			#user{name="Bill", age=34, gender=male},
%%			#user{name="Helen", age=21, gender=female},
%%			#user{name="Kate", age=18, gender=female}
%%		].
%%
%%init() ->
%%  Tid = ets:new(my_table, [{keypos, 2}, named_table, set]),
%%  ets:insert(Tid, get_users()),
%%  Tid.
%%
%%
%%get_young() ->
%%  MS = ets:fun2ms(fun(#user{age=Age} = User) when Age<25 -> User end    % sugar. not F = fun(...)
%%                  ),
%%  ets:select(my_table, MS).
%%
%%-------------------------------------------

%%-record(signal, {
%%  id,
%%  source,
%%  number
%%}).

get_source() ->
  Sources = [source1, source2, source3, source4, source5],
  lists:nth(rand:uniform(length(Sources)), Sources).


get_num() ->
  rand:uniform(100000).


init() ->
  Tid = ets:new(sig_table, [named_table]),
  lists:foreach(fun(Id) -> ets:insert(sig_table, {Id, get_source(), get_num()}) end, lists:seq(1, 10000)),
  Tid.


dest_table_init() ->
  ets:new(dest_table, [named_table]).


count_record(Source) ->
  length(ets:match(sig_table, {'_', Source, '_'})).


move(Min, Max) ->
  MS = ets:fun2ms(
    fun({_, _, Num} = Signal) when (Num > Min) and (Num < Max) ->
      Signal
    end),
  ets:delete_all_objects(dest_table),
  ets:insert(dest_table, ets:select(sig_table, MS)).

