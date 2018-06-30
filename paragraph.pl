:- module(paragraph,
          [
            lines_to_paragraphs/2
          ]).
:- use_module(attribute, [attribute_tag/3]).

paragraph_tag(AIn, AOut) :-
  A = (paragraphs = true), % TODO
  attribute_tag(A, AIn, AOut).

join_paragraphs(
).


lines_to_paragraphs(
  Lines,
  Paragraphs
) :-
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
  line_bbox(PrevLine, X0),
  line_bbox(Line, X),
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
  line_bbox(PrevLine, X0),
  line_bbox(Line, X),
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
    
