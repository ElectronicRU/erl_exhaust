%%%-------------------------------------------------------------------
%%% @author Alex S.
%%% @copyright (C) 2016, Alex S.
%%% @doc
%%% No-debug version of exhaust internals (much, much less tags).
%%% @end
%%% Created : 2016-07-28 18:46
%%%-------------------------------------------------------------------
-author("Alex S.").

-define(PAIR(X,Y),{X, Y}).
-define(RED_NODE(Next,Meta,E),{Next, Meta, E}).
-define(YELLOW_NODE(Next,E1,E2),{Next, E1, E2}).
-define(GREEN_NODE(Next,Meta,E1,E2,E3),{Next, Meta, E1, E2, E3}).
-define(EMPTY,exhaust_empty).
-define(EXHAUST(S,M),{exhaust,S,M}).

-define(OPZERO(V,E),{V,E}).
-define(OPONE(V,E1,E2),{V,E1,E2}).
-define(OPTWO(V,E1,E2,E3),{V,E1,E2,E3}).