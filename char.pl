:- module(char, []).
:- use_module(document, [leaf_rule/2]).


document:leaf_rule(
  element(char, Attrs, _Children),
  char(C)
) :-
  member(c = C, Attrs).
