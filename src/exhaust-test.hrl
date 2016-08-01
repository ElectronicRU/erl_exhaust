%%%-------------------------------------------------------------------
%%% @author Alex S.
%%% @copyright (C) 2016, Alex S.
%%% @doc
%%% Test version of exhaust internals.
%%% @end
%%% Created : 2016-07-28 13:10
%%%-------------------------------------------------------------------
-author("Alex S.").

-define(PAIR(X,Y),{X, Y}).
-define(RED_NODE(Next,Meta,E),[{red, Next, E}|Meta]).
-define(YELLOW_NODE(Next,E1,E2),[{yellow, E1, E2}|Next]).
-define(GREEN_NODE(Next,Meta,E1,E2,E3),[{green, Next, E1, E2, E3}|Meta]).
-define(EMPTY,[]).
-define(EXHAUST(S,M),{exhaust,S,M}).

-define(OPZERO(V,E),{V,E}).
-define(OPONE(V,E1,E2),{V,E1,E2}).
-define(OPTWO(V,E1,E2,E3),{V,E1,E2,E3}).