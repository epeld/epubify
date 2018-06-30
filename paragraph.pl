:- module(paragraph,
          [
            lines_to_paragraphs/2,
            join_paragraphs/3,
            is_paragraphed/1
          ]).
:- use_module(attribute,
              [
                attribute_tag/3,
                element_attribute/2
              ]).

is_paragraphed(Element) :-
  element_attribute(Element, A),
  paragraph_attr(A).

paragraph_attr(paragraphs = true).

paragraph_tag(AIn, AOut) :-
  paragraph_attr(paragraphs = true),
  attribute_tag(A, AIn, AOut).


join_paragraphs(
  element(paragraph, _A, B),
  element(paragraph, _A2, B2),
  Out
) :-
  % A paragraph can be joined to the one before it if:
  % it does not seem to be indented relative to the last line
  % in the previous paragraph
  all_but_last(B, BAllButLast, BLast),
  B2 = [B2First | B2AllButFirst],

  % TODO add section here verifying fonts match
  % or else, don't join
  
  join_paragraphs(BLast, B2First, BAllButLast, B2AllButFirst, Out).

join_paragraphs(BLast, B2First, BAllButLast, B2AllButFirst, Out) :-
  element_bbox_x(BLast, X0),
  element_bbox_x(B2First, X),
  X0 >= X,

  % Second line is not indented relative to first, so join paragraphs
  append([BAllButLast, [BLast], [B2First], B2AllButFirst], Joined),
  Out = [
    element(paragraph, [joined = true, paragraphs = true], Joined)
  ].

join_paragraphs(BLast, B2First, BAllButLast, B2AllButFirst, Out) :-
  element_bbox_x(BLast, X0),
  element_bbox_x(B2First, X),
  X0 < X,

  % Second paragraph is indented. Don't join
  append([BAllButLast, BLast], P1),
  append([B2First, B2AllButFirst], P2),
  Out = [
    element(paragraph, [paragraphs = true, joined = false], P1),
    element(paragraph, [paragraphs = true, joined = false], P2)
  ].


lines_to_paragraphs(
  Lines,
  Paragraphs
) :-
  % TODO: need to reverse the lines inside the paragraph here
  % AND convert paragraph(C) -> element(paragraph, [], C)
  lines_to_paragraphs(
    none,
    Lines,
    [],
    Paragraphs
  ).


% if a line is indented more than the previous
% then it is the start of a paragraph
lines_to_paragraphs(
  PrevLine,
  [Line | Rest],
  Paragraphs,
  ParagraphsOut
) :-
  element_bbox_x(PrevLine, X0),
  element_bbox_x(Line, X),
  X0 < X,
  lines_to_paragraphs(
    Line,
    Rest,
    [paragraph([Line]) | Paragraphs],
    ParagraphsOut
  ).

% if a line is indented the same as the previous
% then it is the same paragraph
lines_to_paragraphs(
  PrevLine,
  [Line | Rest],
  [paragraph(Lines) | Paragraphs],
  ParagraphsOut
) :-
  element_bbox_x(PrevLine, X0),
  element_bbox_x(Line, X),
  X =< X0,
  lines_to_paragraphs(
    Line,
    Rest,
    [paragraph([Line | Lines]) | Paragraphs],
    ParagraphsOut
  ).

% The first line is assumed to be a new paragraph
% (we can correct this error on page-level later)
lines_to_paragraphs(
  none,
  [Line | Rest],
  [],
  ParagraphsOut
) :-
  lines_to_paragraphs(
    Line,
    Rest,
    [paragraph([Line])],
    ParagraphsOut
  ).
    
