%%%-------------------------------------------------------------------
%%% @author Alex S.
%%% @copyright (C) 2016, Alex S.
%%% @doc
%%%
%%% @end
%%% Created : 2016-07-28 13:10
%%%-------------------------------------------------------------------
-module(exhaust).
-author("Alex S.").

%% API exports
-compile(export_all).
-export_type([exhaust/1]).
-export([build/1, drop/2]).

-include_lib("eunit/include/eunit.hrl").

-ifndef(PRODUCTION).
-include("exhaust-test.hrl").
-else.
-include("exhaust-notest.hrl").
-endif.

%%% This is more or less an analogue of your usual binomial tree using
%%% Redundant Binary Representation as outlined by
%%% "Purely Functional, Real-Time Deques with Catenation" (Kaplan, Tarjan, 1996).
%%% As we only need to pop, the structure is greatly simplified,
%%% but their method of normalization holds.
%%% We combine their approach, using Red = 1, Yellow = 2, Green = 3,
%%% with the additional of optional `last node`, which is of size 1
%%%(or, essentially, 0 when absent), but is always considered green.
%%% It's easy to see that it doesn't break the properties of RBR
%%%(and indeed, we could theoretically make it any length at all,
%%% if not for dropping), as we can always painlessly borrow from it
%%% if it is present. It is not represented explicitly and is instead just
%%% a red node with two empty links. This is perhaps slightly less memory
%%% efficient, but there are less checks going on in the code.
%%%
%%% Regular RBR number is an RBR number that for every Red digit
%%%(0 in our case) has a less-significant Green digit separated only by
%%% Yellow digits. K/T 1996 calls 0 Red and 2 Green which works better
%%% for the purposes of addition, but we are subtracting (dropping).
%%%
%%% We use the binary encoding of binomial trees, where ?PAIR(V,L,R)
%%% denotes a tree of order V with big subtree L and the rest (including root)
%%% as R; this has the rather unimportant property of burying the root,
%%% but since we never actually look at the values, we are able to use what is
%%% essentially full binary trees.

-type pair(T) :: ?PAIR(T, T).

-type desc(T) :: pair(T) | desc(pair(T)).
-type udesc(T) :: udesc(T).
-type latestack(T) :: metastack(desc(T)).
-type stack(T) :: ?YELLOW_NODE(stack(pair(T)),T,T) | ?EMPTY.
-type metastack(T) ::
%% red digit
?RED_NODE(stack(pair(T)), latestack(T), T) |
%% green digit
?GREEN_NODE(stack(pair(T)), latestack(T), T, T, T) |
?EMPTY.

-type exhaust(T) :: ?EXHAUST(stack(T), latestack(T)).

%%% There used to be a bunch of stuff about why I don't interleave stacks
%%% here, but it turned out to be a bunch of balderdash.
%%%
%%% Turns out that tuple size can carry a lot of information, and in PRODUCTION
%%% build, it makes our coffee for us, pretty much.
%%% That amounts to 6 words for Green, 4 for Yellow and 3 for Red;
%%% the anecdotal evidence is that numbers tend to be about 50% yellow,
%%% which is no worse than 5 per digit in a log-3 setting!
%%% So, we use about 5 log_3(N) words for N elements (plus up to a constant amount),
%%% which is amazing, if you ask me.

%%====================================================================
%% API functions
%%====================================================================

-spec build([T]) -> exhaust(T).
build(List) ->
  Length = length(List),
  build_green(List, Length, []).

%%% Multidrop is not described in K/T 1996, and we do not use
%%% any kind of tagging, as the level alone of a node can be used to
%%% determine its size (indeed, for node V it is 2^V).
%%%
%%% Our approach is twofold. At first, we dynamically subtract a given
%%% number from our representation: 1 or 2 from green levels, 0 or 1 from yellow,
%%%(depending on parity)
%%% and 0 from red. It is achieved by levels passing up how much they would like
%%% their parent to take upon themselves, and passing down how much they weren't
%%% in fact able to take. It is trivial to show that whatever gets passed out of the least
%%% significant node is how many entries were asked to be dropped, but are not present.
%%% Secondly, we run over any newly-created metastack entries (possibly also the next one,
%%% as it could have lost cover) and replace contiguous sequences of Red with Yellow...Green.
%%% It is essentially a generalization of a method outlined in K/T 1996,
%%% and indeed degenerates to it when at most 1 new metastack entry was created.


-spec drop(HowMuch :: pos_integer(), exhaust(T)) ->
  {ok, LastDropped :: T, Rest :: exhaust(T)} |
  {too_much, Remain :: pos_integer(), LastDropped :: T, Rest :: exhaust(T)}.
drop(0, _) ->
  error(badarg);
drop(HowMuch, ?EXHAUST(Stack,Meta)) ->
  {Rest, Last, Stack1, Meta1} = sub(HowMuch, Stack, [], Meta),
  Meta2 = straighten(Meta1),
  case Rest of
    0 ->
      {ok, Last, ?EXHAUST(Stack1,Meta2)};
    _ ->
      {too_much, Rest, Last, ?EXHAUST(Stack1,Meta2)}
  end.



%%====================================================================
%% Internal functions
%%====================================================================
-type pairtriple(U) :: {U, U} | {U, U, U}.
-type gryellow(T) :: pairtriple(T) | gryellow(pair(T)).
-spec build_green([T], non_neg_integer(), [gryellow(T)]) ->
  exhaust(T).
%% We want to make as little metastacks as possible.
%% We have a choice between red and green, as both are 0 modulo 2,
%% however, there is no benefit to ever choosing red, as we can encode the
%% entire thing rather compactly using only green and yellow, and maybe one red.
build_green([], 0, Stack) ->
  build_finish(Stack, ?EMPTY, ?EMPTY);
build_green([E1], 1, Stack) ->
  Last = ?RED_NODE(?EMPTY,?EMPTY,E1),
  build_finish(Stack, ?EMPTY, Last);
%% making a yellow digit (2)
build_green([E1, E2 | Elements], NElements, Stack) when NElements rem 2 == 0 ->
  Elements1 = build_pairs(Elements),
  build_green(Elements1, NElements div 2 - 1, [{E1, E2}|Stack]);
%% making a green digit (3)
build_green([E1, E2, E3 | Elements], NElements, Stack) ->
  Elements1 = build_pairs(Elements),
  build_green(Elements1, NElements div 2 - 1, [{E1, E2, E3} | Stack]).

-spec build_finish([udesc(T)], stack(udesc(T)), metastack(udesc(T))) ->
  exhaust(T).
%% as of recent Erlangs, body-recursive builder functions are about as fast as tail-recursive ones.
build_finish([], Stack, Metastack) ->
  ?EXHAUST(Stack, Metastack);
%% green digit
build_finish([{E1, E2, E3} | Rest], Stack, Metastack) ->
  Green = ?GREEN_NODE(Stack, Metastack, E1, E2, E3),
  build_finish(Rest, ?EMPTY, Green);
%% yellow digit
build_finish([{E1, E2} | Rest], Stack, Metastack) ->
  Yellow = ?YELLOW_NODE(Stack, E1, E2),
  build_finish(Rest, Yellow, Metastack).

-spec build_pairs([T]) -> [pair(T)].
build_pairs([]) -> [];
build_pairs([Element1, Element2 | Rest]) ->
  [?PAIR(Element1, Element2) | build_pairs(Rest)].

-type operation(T) :: ?OPZERO(0 | 1, T) | ?OPONE(0 | 1, T, T) | ?OPTWO(1 | 2, T, T, T).
-type opstack(T) :: [operation(udesc(T))].
-type execute_result(T) :: {Rest :: non_neg_integer(), LastDropped :: T | ?EMPTY,
                            stack(T), metastack(T)}.

-spec sub(HowMuch :: non_neg_integer(), Stack :: stack(U), Ops, Meta :: latestack(U)) ->
  execute_result(T) when U :: udesc(T), Ops :: opstack(T).
%% this first to create LAST_NODE.
sub(N, ?EMPTY, Ops, ?EMPTY) ->
  cull(N, Ops, ?EMPTY);
sub(0, Stack, Ops, Meta) ->
  execute(0, Ops, ?EMPTY, Stack, Meta);
sub(N, ?YELLOW_NODE(Next, E1, E2), Ops, Meta) ->
  sub(N div 2, Next, [?OPONE(N rem 2,E1,E2) | Ops], Meta);
sub(N, ?EMPTY, Ops, ?GREEN_NODE(Next, NextMeta, E1, E2, E3)) ->
  Rem = 2 - (N rem 2), %% this is necessary because we always want to take away at least 1
  sub((N - Rem) div 2, Next, [?OPTWO(Rem,E1,E2,E3) | Ops], NextMeta);
sub(N, ?EMPTY, Ops, ?RED_NODE(Next, NextMeta, E1)) ->
  sub(N div 2, Next, [?OPZERO(N rem 2,E1) | Ops], NextMeta).

%% Here the stuff gets actually interesting.
-type cache(T) :: ?EMPTY | pair(T).
-type cacheout(T) :: ?EMPTY | T.

%% This simulates a max-size-2 queue, putting from the left, getting from the right.
%% Putting as much elements into cache as many arguments are;
%% getting as much as the name of the function suggests.
-spec qput(operation(T), cache(T)) -> cacheout(T).
qput(_, ?PAIR(_,Right)) ->
  Right;
qput(?OPZERO(_,E1), ?EMPTY) ->
  E1;
qput(?OPONE(_,_,E2), ?EMPTY) ->
  E2;
qput(?OPTWO(_,_,_,E3), ?EMPTY) ->
  E3.

-type result(T) :: {cacheout(T), T} | {cacheout(T), T, T}.
-spec qshift(operation(T), Into :: 0 | 1 | 2, cache(T)) -> result(T).
qshift(?OPZERO(_,_), 0, ?PAIR(Left,Right)) ->
  {Left, Right};
qshift(?OPONE(_,_,E2), 0, ?PAIR(Left,Right)) ->
  {E2, Left, Right};
qshift(?OPONE(_,_,_), 1, ?PAIR(Left,Right)) ->
  {Left, Right};
qshift(?OPTWO(_,_,_,E3), 1, ?PAIR(Left,Right)) ->
  {E3, Left, Right};
qshift(?OPTWO(_,_,_,_), 2, ?PAIR(Left,Right)) ->
  {Left, Right};
qshift(?OPZERO(_,E1), 0, ?EMPTY) ->
  {?EMPTY, E1};
qshift(?OPONE(_,E1,E2), 1, ?EMPTY) ->
  {E1, E2};
qshift(?OPTWO(_,E1,E2,E3), 1, ?EMPTY) ->
  {E1, E2, E3};
qshift(?OPTWO(_,_,E2,E3), 2, ?EMPTY) ->
  {E2, E3}.

-spec shift_op(operation(T), HowMuch :: non_neg_integer(), cache(T)) ->
  {Rest :: non_neg_integer(), result(T)}.
shift_op(?OPZERO(V,_) = Op, HowMuch, Cache) ->
  {HowMuch + V, qshift(Op, 0, Cache)};
shift_op(?OPONE(0,_,_) = Op, 0, Cache) ->
  {0, qshift(Op, 0, Cache)};
shift_op(?OPONE(V,_,_) = Op, HowMuch, Cache) ->
  {HowMuch + V - 1, qshift(Op, 1, Cache)};
shift_op(?OPTWO(V,_,_,_) = Op, HowMuch, Cache) when HowMuch + V < 2 ->
  {0, qshift(Op, HowMuch + V, Cache)};
shift_op(?OPTWO(V,_,_,_) = Op, HowMuch, Cache) ->
  {HowMuch + V - 2, qshift(Op, 2, Cache)}.

-spec cull_op(operation(T), HowMuch :: non_neg_integer(), cache(T)) ->
  {Rest :: non_neg_integer(), {cacheout(T)}} |
  {Rest :: non_neg_integer(), result(T)}.
cull_op(?OPZERO(V,_) = Op, HowMuch, Cache) when HowMuch + V >= 1 ->
  {HowMuch + V - 1, {qput(Op, Cache)}};
cull_op(?OPONE(V,_,_) = Op, HowMuch, Cache) when HowMuch + V >= 2 ->
  {HowMuch + V - 2, {qput(Op, Cache)}};
cull_op(?OPTWO(V,_,_,_) = Op, HowMuch, Cache) when HowMuch + V >= 3 ->
  {HowMuch + V - 3, {qput(Op, Cache)}};
cull_op(Op, HowMuch, Cache) ->
  shift_op(Op, HowMuch, Cache).

%% Rebuild from an empty base, dropping empties.
-spec cull(DropFirst :: non_neg_integer(), opstack(T), cacheout(desc(T))) ->
  execute_result(T).
cull(N, [Op | Tail], Cache) ->
  {Rest, Result} = cull_op(Op, N * 2, Cache),
  case Result of
    {CacheOut} ->
      cull(Rest, Tail, CacheOut);
    {CacheOut, E1} ->
      execute(N, Tail, CacheOut, ?EMPTY, ?RED_NODE(?EMPTY,?EMPTY,E1));
    {CacheOut, E1,E2} ->
      execute(N, Tail, CacheOut, ?YELLOW_NODE(?EMPTY,E1,E2), ?EMPTY)
  end;
%% the stack is empty now
cull(N, [], Cache) ->
  {N, Cache, ?EMPTY, ?EMPTY}.

-spec execute(DropFirst :: non_neg_integer(), opstack(T), cacheout(desc(T)),
  stack(U), latestack(U)) ->
  execute_result(T) when U :: udesc(T).

execute(N, [Op | Tail], Cache, Stack, Metastack) ->
  {Rest, Result} = shift_op(Op, N, Cache),
  Rest1 = Rest * 2,
  case Result of
    {CacheOut, E1} ->
      Red = ?RED_NODE(Stack,Metastack,E1),
      execute(Rest1, Tail, CacheOut, ?EMPTY, Red);
    {CacheOut, E1, E2} ->
      Yellow = ?YELLOW_NODE(Stack,E1,E2),
      execute(Rest1, Tail, CacheOut, Yellow, Metastack)
  end;
execute(N, [], Cache, Stack, Metastack) ->
  {N, Cache, Stack, Metastack}.


%% we only create Red, Yellow and Last nodes ourselves, thus we
%% don't exactly plan on encountering any (barring one)
%% Green nodes here.
-spec straighten(metastack(T)) -> metastack(T).
straighten(?RED_NODE(?EMPTY,?EMPTY,_) = Last) ->
  Last;
straighten(?RED_NODE(?EMPTY,Meta,E1)) ->
  {?PAIR(E2, E3), Stack, Metastack} = borrow(Meta),
  ?GREEN_NODE(Stack,Metastack,E1,E2,E3);
straighten(?RED_NODE(Stack,Meta,E1)) ->
  ?YELLOW_NODE(Stack1,?PAIR(E2,E3),E02) = Stack,
  %% we gonna make a red node here, march on.
  Meta1 = straighten(Meta),
  RedMeta = ?RED_NODE(Stack1,Meta1,E02),
  ?GREEN_NODE(?EMPTY,RedMeta,E1,E2,E3);
straighten(GreenOrEmpty) ->
  GreenOrEmpty.

-spec borrow(metastack(pair(T))) ->
  {T, stack(T), latestack(T)}.
borrow(?RED_NODE(?EMPTY,?EMPTY,E1)) ->
  {E1, ?EMPTY, ?EMPTY};
borrow(?RED_NODE(?EMPTY,Meta,E1)) ->
  {?PAIR(E2, E3), Stack, Metastack} = borrow(Meta),
  Yellow = ?YELLOW_NODE(Stack, E2, E3),
  {E1, Yellow, Metastack};
borrow(?RED_NODE(Stack,Meta,E1)) ->
  ?YELLOW_NODE(Stack1,?PAIR(E2,E3),E4) = Stack,
  Meta1 = straighten(Meta),
  Yellow = ?YELLOW_NODE(?EMPTY,E2,E3),
  %% small dragons: don't leave
  {E1, Yellow, ?RED_NODE(Stack1, Meta1, E4)};
borrow(?GREEN_NODE(Stack,Meta,E1,E2,E3)) ->
  Yellow = ?YELLOW_NODE(Stack,E2,E3),
  %% at the of borrow, we always create a green node,
  %% and past the natural green, there is no chaos we caused.
  {E1, Yellow, Meta}.


-ifdef(EUNIT).

basic_test() ->
  List = "A quick brown fox jumps over a lazy dog.",
  Exhaust = build(List),
  Lambda = fun
    Lambda({ok, E, Ex}, Acc) -> Lambda(drop(1, Ex), [E | Acc]);
    Lambda({too_much, 1, _, _}, Acc) -> Acc
  end,
  ?assertEqual(List, lists:reverse(Lambda(drop(1, Exhaust), []))).

-endif.


%% Phew! If you're reading this, you've earned yourself a free hug.











