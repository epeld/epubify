:- module(block,
          [
            block_rule/2,
          ]).
:- use_module(attribute, [attribute_tag/3]).
:- use_module(paragraph, [lines_to_paragraphs/2]).

block_rule(
  element(block, Attrs, Children),
  element(block, AttrsOut, Transformed)
) :-
  paragraph_attr(Attr),
  attribute_tag(Attr, Attrs, AttrsOut),
  assert_all(is_line, Children),

  lines_to_paragraphs(Children, Transformed).


% TODO move to line.pl?
is_line(element(line, _A, _C)).

is_paragraphed_block(
  element(block, A, _C)
) :-
  paragraph_attr(Attr),
  member(Attrs, A).


paragraph_attr(paragraphs = true).


assert_all(Goal, [I | Rest]) :-
  !,
  Items = [I | Rest],
  (
    exclude(Goal, Items, []) *->
    true
  ; throw(bad_assumption(every_element, Goal, Items))
  ).

assert_all(Goal, []).
