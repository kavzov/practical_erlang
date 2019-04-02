-module(extra).

-export([init/0, group_by_gender/1, group_by/2, group_age_fn/0, group_gender_fn/0]).
-include_lib("eunit/include/eunit.hrl").


init() ->
	[
		{user, "Bob", 29, male},
		{user, "Molly", 12, female},
		{user, "Bill", 34, male},
		{user, "Helen", 21, female},
		{user, "Patrick", 65, male},
		{user, "Laura", 55, female},
		{user, "Dan", 15, male},
		{user, "Kate", 18, female}
	].


group_by(GroupFn, Users) ->
	UsersPL = [{GroupFn(User), User} || User <- Users],
	GroupCategories = proplists:get_keys(UsersPL),
	lists:foldl(
		fun(GC, M) ->
			maps:put(GC, proplists:get_all_values(GC, UsersPL), M)
		end,
		#{},
		GroupCategories
	).


group_by_gender(Users) ->
	{Males, Females} = lists:foldl(
		fun(User, {Acc1, Acc2}) ->
			case User of
				{user, _, _, male} -> {[User | Acc1], Acc2};
				{user, _, _, female} -> {Acc1, [User | Acc2]}
			end
		end,
		{[], []},
		Users
	),
	#{male => Males, female => Females}.


%% Tests

group_age_fn() ->
	fun({user, _, Age, _}) ->
		if
			Age =< 15 -> child;
			(Age > 15) and (Age =< 25) -> young;
			(Age > 25) and (Age =< 50) -> middle;
			Age > 50 -> old
		end
	end.

group_gender_fn() ->
	fun({user, _, _, Gender}) ->
		Gender
	end.


group_by_test() ->
	AgeGroupResults =
	   #{
			child => [{user,"Molly",12,female},{user,"Dan",15,male}],
  			young => [{user,"Helen",21,female},{user,"Kate",18,female}],
  			middle => [{user,"Bob",29,male},{user,"Bill",34,male}],
  			old => [{user,"Patrick",65,male},{user,"Laura",55,female}]
  		},
  	GenderGroupResults =
  	   #{
		  	female => [{user,"Molly",12,female},
		       		   {user,"Helen",21,female},
		       		   {user,"Laura",55,female},
		       		   {user,"Kate",18,female}],
		  	male => [{user,"Bob",29,male},
		       		 {user,"Bill",34,male},
		       		 {user,"Patrick",65,male},
		       		 {user,"Dan",15,male}]
	    },
  	?assertEqual(AgeGroupResults, group_by(group_age_fn(), init())),
  	?assertEqual(GenderGroupResults, group_by(group_gender_fn(), init())),
  	ok.

group_by_gender_test() ->
	Results =
	  #{
		female =>
	      [{user,"Kate",18,female},
	       {user,"Laura",55,female},
	       {user,"Helen",21,female},
	       {user,"Molly",12,female}],
	    male =>
	      [{user,"Dan",15,male},
	       {user,"Patrick",65,male},
	       {user,"Bill",34,male},
	       {user,"Bob",29,male}]
	    },
    ?assertEqual(Results, group_by_gender(init())),
    ok.
