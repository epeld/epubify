:- module(paragraph,
          [
            lines_to_paragraphs/2,
          ]).
% :- use_module(attribute, [attribute_tag/3]).



% if a line is indented more than the previous
% then it is the start of a paragraph
%
% if a line is indented the same as the previous
% then it is the same paragraph
%
% if the initial line in a block is indented same
% as the next, then it is a continued paragraph
% from previous block


lines_to_paragraphs(
  PrevLine,
  [Line | Rest]
) :-
  line_bbox(PrevLine, X0),
  line_bbox(Line, X),
  false.
