:- module(page,
          [
            % TODO
          ]).
:- use_module(attribute,
              [
                attribute_tag/2,
                element_attribute/2,
                is_paragraphed/1,
                paragraph_tag/2
              ]).
:- use_module(block, [join_blocks/3]).
:- use_module(list, [all_but_last/3]).


page_rule(
  element(page, Attrs, Children),
  element(page, AttrsOut, Transformed),
) :-
  maplist(
    is_paragraphed,
    Children
  ),
  paragraph_tag(Attrs, AttrsOut),

  Children = [Page | Pages],

  % Note: possible slow down due to always traversing
  % the accumulator to find the last block
  foldl(join_pages_rev, Pages, Page, Transformed).


% Flipped version of join_pages, to work with foldl
join_pages_rev(Page2, Page1, Joined) :-
  join_pages(Page1, Page2, Joined).


% Joining two pages into one
join_pages(
  element(page, A, B),
  element(page, A2, B2),
  element(page, [joined], Joined)
) :-
  % We will keep all children
  % except possibly the last in B and first in B2
  % which might have to be joined

  all_but_last(B, BAllButLast, BLast),
  B2 = [B2First | B2AllButFirst],
  join_blocks(B, B2, BB2),

  append([BAllButLast, BB2, B2AllButFirst], Joined).


  
