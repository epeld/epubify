:- module(block,
          [
            block_rule/2,
            is_paragraphed_block/1,
            join_blocks/3
          ]).
:- use_module(attribute, [attribute_tag/3]).
:- use_module(paragraph,
              [
                lines_to_paragraphs/2,
                join_paragraphs/3,
                paragraph_tag/2,
                is_paragraphed/1
              ]).
:- use_module(list, [all_but_last/3]).

join_blocks(
  element(block, A, B),
  element(block, A2, B2),
  element(block, AJoined, BJoined)
) :-
  is_paragraphed(element(block, A, B)),
  is_paragraphed(element(block, A2, B2)),

  % We will keep all children
  % except possibly the last in B and first in B2
  % which might have to be joined
  all_but_last(B, BAllButLast, BLast),
  B2 = [B2First | B2AllButFirst],

  join_paragraphs(B, B2, BB2),

  append([BAllButLast, BB2, B2AllButFirst], Joined).
  

block_rule(
  element(block, Attrs, Children),
  element(block, AttrsOut, Transformed)
) :-
  paragraph_tag(Attrs, AttrsOut),
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
