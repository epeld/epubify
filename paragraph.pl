:- module(paragraph,
          [
            lines_to_paragraphs/2
          ]).
% :- use_module(attribute, [attribute_tag/3]).




%

%



lines_to_paragraphs(
  LeftMargin,
  Lines,
  Paragraphs
) :-
  lines_to_paragraphs(
    LeftMargin,
    none,
    Lines,
    [],
    Paragraphs
  ).


% if a line is indented more than the previous
% then it is the start of a paragraph
lines_to_paragraphs(
  LeftMargin,
  PrevLine,
  [Line | Rest],
  Paragraphs,
  ParagraphsOut
) :-
  line_bbox(PrevLine, X0),
  line_bbox(Line, X),
  X0 < X,
  lines_to_paragraphs(
    LeftMargin,
    Line,
    Rest,
    [paragraph([Line]) | Paragraphs],
    ParagraphsOut
  ).

% if a line is indented the same as the previous
% then it is the same paragraph
lines_to_paragraphs(
  LeftMargin,
  PrevLine,
  [Line | Rest],
  [paragraph(Lines) | Paragraphs],
  ParagraphsOut
) :-
  line_bbox(PrevLine, X0),
  line_bbox(Line, X),
  X =< X0,
  lines_to_paragraphs(
    LeftMargin,
    Line,
    Rest,
    [paragraph([Line | Lines]) | Paragraphs],
    ParagraphsOut
  ).
    

% TODO remove this rule and use the block's LeftMargin instead
% if the initial line in a block is indented same
% as the next, then it is a continued paragraph
% from previous block
lines_to_paragraphs(
  LeftMargin,
  none,
  [Line, NextLine | Rest],
  [],
  ParagraphsOut
) :-
  line_bbox(Line, X0),
  line_bbox(NextLine, X),
  X = X0,
  lines_to_paragraphs(
    LeftMargin,
    NextLine,
    Rest,
    [paragraph([NextLine, Line, continued])],
    ParagraphsOut
  ).


    
