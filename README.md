# `erl_exhaust`

## What is it?
It's a persistent list view that allows you to quickly drop N first
elements with O(N) asymptotic.

## How is it done?
It's a neat idea based on 
"Purely Functional, Real-Time Deques with Catenation" (Kaplan, Tarjan, 1996)".
The basis for the data structure is your standard, run-of-the-mill
complete binary tree forest; but as that maps to ordinary binary
representation, as `erl_exhaust` maps to redundant binary representation,
meaning digits available are 0, 1, or 2. If you're interested,
`src/exhaust.erl` is relatively well-commented, and I tried to lay out
my design decisions along the way.

## How to use it?
    Exhaust = exhaust:create(List),
    {ok, Element, Exhaust1} = exhaust:drop(N, Exhaust),
    % or
    {error, empty} = exhaust:drop(N, Exhaust),
    %or
    {error, dropped_too_much} = exhaust:drop(N, Exhaust).

In the future, the size function (even cached) might be added if is called for.

## Oh god my head hurts
This was a triumph. I'm making a note here: HUGE SUCCESS.
