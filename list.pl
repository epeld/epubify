:- module(list, [all_but_last/3]).

all_but_last(ListIn, AllButLast, Last) :-
  reverse(ListIn, [Last | AllButLastR]),
  reverse(AllButLastR, AllButLast).
