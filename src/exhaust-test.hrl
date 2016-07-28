%%%-------------------------------------------------------------------
%%% @author Alex S.
%%% @copyright (C) 2016, Alex S.
%%% @doc
%%% Test version of exhaust internals.
%%% @end
%%% Created : 2016-07-28 13:10
%%%-------------------------------------------------------------------
-author("Alex S.").

-define(PAIR(V,X,Y),{exhaust_pair, V, X, Y}).
-define(BINOM_TREE(Pair,Rest),{exhaust_tree, Pair, Rest}).
-define(BINOM_EMPTY,{exhaust_tree, empty}).
-define(MSTACK(Type,Digits,Next),{exhaust_mstack, Type, Digits, Next}).
-define(MSTACK_EMPTY,{exhaust_mstack,empty}).
-define(MASK(X),{exhaust_mask,X}).
