:- module(document,
          [
            all_children/2,
            transformation/2,
            leaf_rule/2,
            post_hierarchical_rule/2,
            pre_hierarchical_rule/2
          ]).
:- use_module(transform, [apply_element_rule/3, apply_singleton_rule/3]).
:- use_module(font, [font_name_style/2]).

:- multifile
    pre_hierarchical_rule/2,
    post_hierarchical_rule/2,
    leaf_rule/2.

hierarchy([document, page, block, line, font]).

hierarchical_element(El) :-
  hierarchy(H),
  member(El, H).


%
% Utils
%
all_children(V, [V]) :-
  V \= element(_1, _2, _3).

all_children(element(_El, _A, C),
             Children) :-
  maplist(all_children, C, Cs),
  append(Cs, Children).

%
% Rules
%

transformation(A, B) :-
  hierarchical_rule(A, B).


hierarchical_rule(El, Out) :-
  \+ hierarchical_element(El),
  leaf_rule(El, Out).

hierarchical_rule(
  element(El, Attrs, Children),
  Out
) :-
  hierarchical_element(El),

  % PRE
  apply_singleton_rule(
    document:pre_hierarchical_rule,
    element(El, Attrs, Children),
    Out1
  ),

  note(El, children),

  % CHILDREN
  Out1 = element(El1, Attrs1, Children1),
  apply_element_rule(
    hierarchical_rule,
    Children1,
    Children2
  ),

  note(El, children(done)),

  % POST
  apply_singleton_rule(
    post_hierarchical_rule,
    element(El1, Attrs1, Children2),
    Out
  ),
  note(El, post).


%
% Pre-rules
%
pre_hierarchical_rule(
  element(El, A, C),
  element(El, [ pre | A], C)
) :- note(El, pre), false.


%
% Notes
%

% for tracing
note(_El) :- true.
note(_El, _Tag) :- true.


