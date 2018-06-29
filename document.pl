:- module(document, [transformation/2]).
:- use_module(transform, [apply_element_rule/3, apply_rule/3]).

hierarchy([document, page, block, line, font]).

hierarchical_element(El) :-
  hierarchy(H),
  member(El, H).


%
% Rules
%

transformation(A, B) :-
  hierarchical_rule(A, B).

hierarchical_rule(
  element(El, Attrs, Children),
  Out
) :-
  hierarchical_element(El),

  % PRE
  apply_rule(
    pre_hierarchical_rule,
    element(El, Attrs, Children),
    Out1,
  ),

  % CHILDREN
  Out1 = element(El1, Attrs1, Children1),
  transform(
    hierarchical_rule,
    Children1,
    Children2
  ),

  % POST
  apply_rule(
    post_hierarchical_rule
    element(El1, Attrs1, Children2),
    Out
  ).
