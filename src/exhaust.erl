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

-ifdef(TEST).
-include("exhaust-test.hrl").

basic_test() ->
  List = "A quick brown fox jumps over a lazy dog.",
  Exhaust = build(List),
  Lambda = fun
    Lambda({ok, E, Ex}, Acc) -> Lambda(drop(1, Ex), [E | Acc]);
    Lambda({error, empty}, Acc) -> Acc
  end,
  ?assertEqual(List, lists:reverse(Lambda(drop(1, Exhaust), []))).
-else.
-include("exhaust-notest.hrl").
-endif.

-type pair(T) :: ?PAIR(pos_integer(), T, T).
-type tree(T) :: ?BINOM_EMPTY | ?BINOM_TREE(T, tree(pair(T))).

-type metatree(T) ::
%% yellow digit
?BINOM_TREE(?MASK(T), tree(pair(T))) |
%% green digit
?BINOM_TREE(pair(T), tree(pair(T))) |
%% red digit
tree(pair(T)).
-type udesc(T) :: T | udesc(pair(T)).
-type desc(T) :: udesc(pair(T)).
-type color() :: green | red.
-type latestack(T) :: metastack(desc(T)).
-type metastack(T) :: ?MSTACK_EMPTY | ?MSTACK(color(), metatree(T), latestack(T)).

-type exhaust(T) :: {exhaust, metastack(T)}.

%%% this is more or less an analogue of your usual binomial tree using
%%% Redundant Binary Representation as outlined by
%%% "Purely Functional, Real-Time Deques with Catenation" (Kaplan, Tarjan, 1996).
%%% As we only need to pop, the structure is greatly simplified,
%%% but their method of normalization holds.
%%% We combine their approach, using Red = 0, Yellow = 1, Green = 2,
%%% with the property of binomial trees to skip over empty levels.
%%%
%%% Regular RBR number is an RBR number that for every Red digit
%%% (0 in our case) has a less-significant Green digit separated only by
%%% Yellow digits. K/T 1996 calls 0 Red and 2 Green which works better
%%% for the purposes of addition, but we are subtracting (dropping).
%%%
%%% We use the binary encoding of binomial trees, where ?PAIR(V,L,R)
%%% denotes a tree of order V with big subtree L and the rest (including root)
%%% as R; this has the rather unimportant property of burying the root,
%%% but since we never actually look at the values, we are able to use what is
%%% essentially full binary trees.
%%% !!!NB!!! ?PAIR(V+1,L,R) also represents a green digit of order V;
%%% this is INTENTIONAL, as it is trivial then to convert irregular
%%% 1-0 to regular 0-2, and we can always handle the 1-1 or 1-2 coinciding by looking
%%% at two digits at once.
%%%
%%% Metastack entries can begin with either Red or Green, barring the head one
%%% which can begin with Yellow of order 0. This is the only Yellow that we'll ever wrap,
%%% because we want to support arbitrary user data.
%%% Otherwise, if a metastack entry would begin with Yellow of level V, it
%%% actually begins with Red of level V - 1.
%%%
%%% Distinguishing between cases of metastack can be a little bit tricky.
%%% If the stack contains at least two entries, we compare their levels; if they are
%%% equal, it's Green Yellow, otherwise (implied Red) Yellow Yellow.
%%% If the stack contains only one entry, it's either Red Yellow, or Green.
%%% If it's at the end, then it's ambigious, but it is safe to rewrite 10 as 2 at the end.
%%% Otherwise, it involves inspecting the hea dof the next stack.
%%% However, it's not very practical to do so, and as we use separate structures for
%%% metastack, simply annotating new entries at time of creation is much easier.
%%% Finally, if the stack is empty, it means Red followed by Green (or nothing);
%%% an empty stack is dropped at the end.


%%====================================================================
%% API functions
%%====================================================================

-spec build([T]) -> exhaust(T).
build(List) ->
  Length = length(List),
  if
    Length rem 2 == 0 ->
      {exhaust, build_green(List, Length, [], 0)};
    true ->
      %% mask the last element
      [Element | Rest] = List,
      {exhaust, build_green([?MASK(Element) | Rest], Length, [], 0)}
  end.

%%% Multidrop is not described in K/T 1996, and we do not use
%%% any kind of tagging, as the level alone of a node can be used to
%%% determine its size (indeed, for node V it is 2^V).
%%% Instead, let us consider a recursive approach.
%%% To drop X from a number, we first drop Y \in {0, 1, 2} so that
%%% X - Y divides 2, and then recursively drop (X - Y) div 2
%%% from the next digits. Sometimes, we must carry when
%%% the result would be less than 0, or when it would be 0 and we cannot afford 0
%%% in this position.
%%% The choices we make are rather trivial. We start in a state in which
%%% we cannot allow Red, converting thus Green -> Yellow and Yellow -> Green + borrow,
%%% or leaving everything as is, depending on the odd-even.
%%% Then, once we have made Green at least once, we have an option to convert the
%%% next Yellow to Red (with no borrow) or Green to Red (with carry!).
%%% It is always a good thing to do, as this stops the propagation earlier, bringing
%% the number closer to zero, and less propagation means less copying.

-spec drop(HowMuch :: pos_integer(), exhaust(T)) ->
  {ok, LastDropped :: T, Rest :: exhaust(T)} |
  {error, atom()}.
drop(_, {exhaust, ?MSTACK_EMPTY}) ->
  {error, empty};
drop(0, _) ->
  error(badarg);
drop(HowMuch, {exhaust, ?MSTACK(_Type,Tree,MStack)}) ->
  try
    drop_0(Tree, MStack, HowMuch)
  of
    {Element, MStack1} ->
      {ok, Element, {exhaust, MStack1}}
  catch
    E = {error, _}  -> E
  end.


%%====================================================================
%% Internal functions
%%====================================================================

-spec metastack_new(color(), metatree(T), latestack(T)) -> metastack(T).
metastack_new(red, ?BINOM_EMPTY, ?MSTACK_EMPTY) ->
  ?MSTACK_EMPTY;
metastack_new(Type, Stack, MStack) ->
  ?MSTACK(Type, Stack, MStack).

-spec build_green([?MASK(T) | [T]], non_neg_integer(), [udesc(T)], non_neg_integer()) ->
  metastack(T).
%% We want to make as little metastacks as possible.
%% We have a choice between red and green, as both are 0 modulo 2,
%% however, there is no benefit to ever choosing red, as we can encode the
%% entire thing rather compactly using only green and yellow.
build_green([], 0, Stack, Level) ->
  build_finish(Stack, Level, ?BINOM_EMPTY, ?MSTACK_EMPTY);
%% making a green digit (2)
build_green(Elements, NElements, Stack, Level) when NElements rem 2 == 0 ->
  [OurPair | Pairs] = build_pairs(Elements, Level),
  build_green(Pairs, NElements div 2 - 1, [OurPair | Stack], Level + 1);
%% making a yellow digit (1)
build_green([Element | Elements], NElements, Stack, Level) ->
  build_green(build_pairs(Elements, Level), NElements div 2, [Element | Stack], Level + 1).

-spec build_finish([udesc(T)], non_neg_integer(), metatree(udesc(T)), metastack(udesc(T))) ->
  metastack(T).
%% as of recent Erlangs, body-recursive builder functions are about as fast as tail-recursive ones.
build_finish([], _Level, ?BINOM_EMPTY, Metastack) ->
  Metastack;
build_finish([], _Level, Stack, Metastack) ->
  ?MSTACK(red, Stack, Metastack);
%% green digit
build_finish([?PAIR(Level, _, _) = Element | Rest], Level, Stack, Metastack) ->
  Metastack1 = ?MSTACK(green, ?BINOM_TREE(Element, Stack),Metastack),
  build_finish(Rest, Level - 1, ?BINOM_EMPTY, Metastack1);
%% yellow digit
build_finish([Yellow | Rest], Level, Stack, Metastack) ->
  build_finish(Rest, Level - 1, ?BINOM_TREE(Yellow, Stack), Metastack).

-spec build_pairs([T], pos_integer()) -> [pair(T)].
build_pairs([], _) -> [];
build_pairs([Element1, Element2 | Rest], Level) ->
  [?PAIR(Level + 1, Element1, Element2) | build_pairs(Rest, Level)].


-spec drop_0(metatree(T), latestack(T), pos_integer()) -> {T, metastack(T)}.
%% drop from level 0: special mask handling, and HowMuch is always non-0
%% return type is also different for ease of handling
%% case one element
drop_0(?BINOM_TREE(?MASK(Element), ?BINOM_EMPTY),
               ?MSTACK_EMPTY, _HowMuch) ->
  {Element, ?MSTACK_EMPTY};
%% case Yellow, odd
drop_0(?BINOM_TREE(?MASK(Element), BinomRest),
       MStackRest, HowMuch) when HowMuch rem 2 == 1 ->
  %% borrow a green digit
  {Pair, BinRest, MRest} = drop_green(BinomRest, MStackRest, HowMuch div 2 + 1),
  BinomTree = ?BINOM_TREE(Pair, BinRest),
  {Element, ?MSTACK(green,BinomTree,MRest)};
%% case Yellow, even
drop_0(?BINOM_TREE(?MASK(_), BinomRest),
       MStackRest, HowMuch) when HowMuch rem 2 == 0 ->
  %% shift an yellow digit
  {Pair, BinRest, MRest} = drop_red(BinomRest, MStackRest, HowMuch div 2),
  ?PAIR(1, LastDropped, Remaining) = Pair,
  BinomTree = ?BINOM_TREE(?MASK(Remaining), BinRest),
  {LastDropped, ?MSTACK(red,BinomTree,MRest)};
%% case Green, odd
%% small dragons: the change may not propagate far enough,
%% so we apply a constant-time fixing (see K/T 1996 too).
drop_0(?BINOM_TREE(OurPair, BinomRest),
       MStackRest, HowMuch) when HowMuch rem 2 == 1 ->
  {Pair, BinRest, MRest} = drop_red(BinomRest, MStackRest, HowMuch div 2, OurPair),
  ?PAIR(1, LastDropped, Remaining) = Pair,
  BinomTree = ?BINOM_TREE(?MASK(Remaining), BinRest),
  MRest1 = assert_green(MRest),
  {LastDropped, ?MSTACK(red,BinomTree,MRest1)};
%% case Green, even
drop_0(?BINOM_TREE(OurPair, BinomRest),
       MStackRest, HowMuch) when HowMuch rem 2 == 0 ->
  %% borrow a green digit
  {Pair, BinRest, MRest} = drop_green(BinomRest, MStackRest, HowMuch div 2),
  BinomTree = ?BINOM_TREE(Pair,BinRest),
  ?PAIR(1, _, Element) = OurPair,
  {Element, ?MSTACK(green,BinomTree,MRest)}.

%% for the rest, drop_yellow and drop_green:
%% drop_red -> we cannot make a red (after red),
%% drop_green -> we can make a red (after green).
%% spec: Level, TopStack, RestOfMetastack, HowMuch, [DefaultReturn].
%% returns: Dropped, NewTopStack, NewRestOfMetastack.
%% this is intended to walk YELLOW NODES ONLY.
%% all reds are covered by construction, as we only ever make a red after a green.

-spec drop_red(tree(T), latestack(T), non_neg_integer(), T) ->
  {T, tree(T), latestack(T)}.
drop_red(BinomTop, MStackRest, _HowMuch = 0, Default) ->
  {Default, BinomTop, MStackRest};
drop_red(BinomTop, MStackRest, HowMuch, _Default) ->
  drop_red(BinomTop, MStackRest, HowMuch).

-spec drop_red(tree(T), latestack(T), pos_integer()) ->
  {T, tree(T), latestack(T)}.
drop_red(?BINOM_EMPTY, MStackRest, HowMuch) ->
  %% we have made a red where we were able to. The next metastack entry ought to begin with green or yellow.
  force_green(MStackRest, HowMuch);
%% Yellow after Red, even.
drop_red(?BINOM_TREE(_OurPair,BinomRest), MStackRest,
         HowMuch) when HowMuch rem 2 == 0 ->
  {Pair, BinRest, MRest} = drop_red(BinomRest, MStackRest, HowMuch div 2),
  ?PAIR(_, LastDropped, Remaining) = Pair,
  BinomTree = ?BINOM_TREE(Remaining,BinRest),
  {LastDropped, BinomTree, MRest};
%% Yellow after Red, odd.
drop_red(?BINOM_TREE(OurPair,BinomRest), MStackRest,
         HowMuch) when HowMuch rem 2 == 1 ->
  {Pair, BinRest, MRest} = drop_green(BinomRest, MStackRest, HowMuch div 2 + 1),
  BinomTree = ?BINOM_TREE(Pair,BinRest),
  %% here we have made a new metastack entry.
  MRest1 = ?MSTACK(green,BinomTree,MRest),
  {OurPair, ?BINOM_EMPTY, MRest1}.

-spec drop_green(tree(T), latestack(T), non_neg_integer(), T) ->
  {T, tree(T), latestack(T)}.
drop_green(BinomTop, MStackRest, _HowMuch = 0, Default) ->
  {Default, BinomTop, MStackRest};
drop_green(BinomTop, MStackRest, HowMuch, _Default) ->
  drop_green(BinomTop, MStackRest, HowMuch).

-spec drop_green(tree(T), latestack(T), pos_integer()) ->
  {T, tree(T), latestack(T)}.
drop_green(?BINOM_EMPTY, MStackRest, HowMuch) ->
  %% we are after green, we want to make red if we are able to.
  force_red(MStackRest, HowMuch);
%% Yellow after Green, even.
drop_green(?BINOM_TREE(OurPair,BinomRest), MStackRest,
           HowMuch) when HowMuch rem 2 == 0 ->
  {Pair, BinRest, MRest} = drop_green(BinomRest, MStackRest, HowMuch div 2 - 1, OurPair),
  ?PAIR(_, LastDropped, Remaining) = Pair,
  BinomTree = ?BINOM_TREE(Remaining,BinRest),
  {LastDropped, BinomTree, MRest};
%% Yellow after Green, odd.
drop_green(?BINOM_TREE(OurPair,BinomRest), MStackRest,
           HowMuch) when HowMuch rem 2 == 1 ->
  {Pair, BinRest, MRest} = drop_green(BinomRest, MStackRest, HowMuch div 2, OurPair),
  %% dropping the pair, making the red. Check for empty here.
  MRest1 = metastack_new(red, BinRest,MRest),
  {Pair, ?BINOM_EMPTY, MRest1}.

%% helper functions, force last digit to 0
-spec drop_to_zero_red(color(), metatree(T), latestack(T), pos_integer()) ->
  {T, metatree(pair(T)), latestack(T)}.
drop_to_zero_red(red, Binom, MStackRest, HowMuchForRed) ->
  drop_red(Binom, MStackRest, HowMuchForRed);
drop_to_zero_red(green, ?BINOM_TREE(GreenDigit,BinomRest), MStackRest, HowMuchForRed) ->
  drop_red(BinomRest, MStackRest, HowMuchForRed - 1, GreenDigit).

-spec drop_to_zero_green(color(), metatree(T), latestack(T), pos_integer()) ->
  {T, metatree(pair(T)), latestack(T)}.
drop_to_zero_green(red, Binom, MStackRest, HowMuchForRed) ->
  drop_green(Binom, MStackRest, HowMuchForRed);
drop_to_zero_green(green, ?BINOM_TREE(GreenDigit,BinomRest), MStackRest, HowMuchForRed) ->
  drop_green(BinomRest, MStackRest, HowMuchForRed - 1, GreenDigit).


-spec force_red(latestack(T), pos_integer()) ->
  {T, metatree(T), latestack(T)}.
%% spec here: metastack, how much to drop
%% returns: dropped, top stack (if we converted to yellow, to merge), the rest of metastack
force_red(?MSTACK_EMPTY, _HowMuch) ->
  throw({error, dropped_too_much});
%% odd: to yellow.
force_red(?MSTACK(Type,BinomTop,MStackRest), HowMuch) when HowMuch rem 2 == 1 ->
  {DroppedPair, BinRest, MRest} = drop_to_zero_green(Type, BinomTop, MStackRest, HowMuch div 2 + 1),
  ?PAIR(_,Dropped,YellowSpoils) = DroppedPair,
  {Dropped, ?BINOM_TREE(YellowSpoils,BinRest), MRest};
force_red(?MSTACK(Type,BinomTop,MStackRest), HowMuch) when HowMuch rem 2 == 0 ->
  %% NB: the color flips, since we are making a red digit.
  {DroppedPair, BinRest, MRest} = drop_to_zero_red(Type, BinomTop, MStackRest, HowMuch div 2),
  ?PAIR(_,_Annihilated,Dropped) = DroppedPair,
  MRest1 = metastack_new(red, BinRest, MRest),
  {Dropped, ?BINOM_EMPTY, MRest1}.

-spec force_green(latestack(T), pos_integer()) ->
  {T, metatree(T), latestack(T)}.
force_green(?MSTACK_EMPTY, _HowMuch) ->
  throw({error, dropped_too_much});
%% odd: to yellow. Yep, copy/paste with drop_green replaced by drop_red.
force_green(?MSTACK(Type,BinomTop,MStackRest), HowMuch) when HowMuch rem 2 == 1 ->
  {DroppedPair, BinRest, MRest} = drop_to_zero_red(Type, BinomTop, MStackRest, HowMuch div 2 + 1),
  ?PAIR(_,Dropped,YellowSpoils) = DroppedPair,
  {Dropped, ?BINOM_TREE(YellowSpoils,BinRest), MRest};
%% here be dragons: subtract N - 1 to obtain second-to-last pair... then subtract one more.
%% Fortunately, this double-propagates at most O(1) nodes, and the implementation is simple.
force_green(?MSTACK(Type,BinomTop,MStackRest), HowMuch) when HowMuch rem 2 == 0 ->
  %% NOTA BENE: since we borrow 1 less than we should, we actually subtract to RED
  %% first, and then borrow one more for green.
  %% thus, the color does NOT flip.
  {DroppedPair, BinRest, MRest} = drop_to_zero_red(Type, BinomTop, MStackRest, HowMuch div 2),
  ?PAIR(_,_Annihilated,Dropped) = DroppedPair,
  %% okay, this was the pair we dropped, now for the real deal...
  {BorrowedPair, BinRest1, MRest1} = drop_green(BinRest, MRest, 1),
  BinRest2 = ?BINOM_TREE(BorrowedPair, BinRest1),
  {Dropped, ?BINOM_EMPTY, ?MSTACK(green,BinRest2,MRest1)}.

-spec assert_green(latestack(T)) -> latestack(T).
%% This is a function we call when we lose Green in the topmost stack.
%% If the propagation has reached the next stack, this is a no-op,
%% but subtracting e.g. 1 from 012 will need the hero that are we.
%% spec: MStackRest - the second+ stacks.
%% returns: new MStack. Guarantees to never drop anything.
assert_green(?MSTACK_EMPTY) ->
  ?MSTACK_EMPTY;
assert_green(?MSTACK(red,BinomTop, MStackRest)) ->
  {GreenPair, BinRest, MRest} = drop_green(BinomTop, MStackRest, 1),
  ?MSTACK(green,?BINOM_TREE(GreenPair, BinRest), MRest);
assert_green(MStack) ->
  MStack.


%% Phew! If you're reading this, you've earned yourself a free hug.











