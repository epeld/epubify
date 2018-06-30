:- module(document, [transformation/2]).
:- use_module(transform, [apply_element_rule/3]).
:- use_module(font, [font_name_style/2]).

hierarchy([document, page, block, line, font]).

hierarchical_element(El) :-
  hierarchy(H),
  member(El, H).


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
    document:hierarchical_rule,
    Children1,
    Children2
  ),

  note(El, children(done)),

  % POST
  apply_singleton_rule(
    document:post_hierarchical_rule,
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

pre_hierarchical_rule(
  A,
  removed
) :- atom(A).


%
% Post-rules
%

post_hierarchical_rule(
  element(font, Attrs, Children),
  element(font, AttrsOut, Transformed)
) :-
  font_rule(
    element(font, Attrs, Children),
    element(font, AttrsOut, Transformed)
  ).


%
% Leaf-rules
%
leaf_rule('\n', removed).

leaf_rule(element(char, Attrs, _Children), char(C)) :-
  member(c = C, Attrs).


%
% Notes
%

% for tracing
note(_El) :- true.
note(_El, _Tag) :- true.


