:- module(transform, [transform/3]).
:- use_module(font, [font_rule/3]).
:- use_module(bbox, [bbox_x/2]).

%
% Apply a transformation rule to In producing Out.
% the transformation predicate is applied repeatedly
% until it no longer succeeds
%
transform(Goal, In, Out) :- 
  maplist(apply_rule(Goal), In, Out0),
  exclude(=(success(removed)),Out0, Out1),
  maplist(get_element, Out1, Out2),
  (
    (
      member(success(_), Out1)
    )*-> (!, transform(Goal, Out2, Out))
  ; Out2 = Out
  ).


%
% Perform transformation rules recursively
% until no more transformations apply
apply_rule(Goal,Element, Out) :-
  (call(Goal, Element, Out0), Out0 \= Element) *->
    (
      Out = success(Out0)
    )
  ; failure(Element) = Out.

 
get_element(success(R), R).
get_element(failure(R), R).


