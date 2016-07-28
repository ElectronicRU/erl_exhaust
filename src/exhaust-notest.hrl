%%%-------------------------------------------------------------------
%%% @author Alex S.
%%% @copyright (C) 2016, Alex S.
%%% @doc
%%% No-debug version of exhaust internals (much, much less tags).
%%% @end
%%% Created : 2016-07-28 18:46
%%%-------------------------------------------------------------------
-author("Alex S.").

%% Tuples and lists take up the same space. Why the difference? Because it seemed logical.
-define(PAIR(V,X,Y),{X, Y}).
-define(BINOM_TREE(Pair,Rest),[Pair | Rest]).
-define(BINOM_EMPTY,[]).
-define(MSTACK(Type,Digits,Next),{Type,Digits,Next}).
-define(MSTACK_EMPTY,{}).
-define(MASK(X),{X}).
