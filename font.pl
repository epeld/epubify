:- module(font,
          [
            font_rule/2,
            font_name_style/2
          ]).
:- use_module(attribute, [attribute_tag/3]).
:- use_module(document,
              [
                post_hierarchical_rule/2,
                leaf_rule/2
              ]).

font_name_style('CMTI10', italic).
font_name_style('CMBX12', heading(2)).

% halmos.pdf
font_name_style('CMBX10', heading(1)).
font_name_style('CMCSC10', heading(2)).
font_name_style('CMR8', footnote(reference)).
font_name_style('CMR10', regular).
font_name_style('CMR7', footer).
font_name_style('CMR6', footnote(reference)).
font_name_style('CMR9', footnote(text)).


%
% Element transformations
%
document:post_hierarchical_rule(
  element(font, Attrs, Children),
  element(font, AttrsOut, Transformed)
) :-
  font_rule(
    element(font, Attrs, Children),
    element(font, AttrsOut, Transformed)
  ).

% Font-elements typically contain newline atoms
document:leaf_rule('\n', removed).


%
% Font-specific transformations
%
font_rule(El, ElOut) :-
  join_chars(El, ElOut).

font_rule(El, ElOut) :-
  classify_font(El, ElOut).

% join chars into strings as much as possible
join_chars(
  element(font, Attrs, Children),
  element(font, AttrsOut, [Transformed])
) :-
  attribute_tag(joined, Attrs, AttrsOut),
  maplist(arg(1), Children, Transformed0),
  reverse(Transformed0, Transformed1),
  foldl(string_concat, Transformed1, '', Transformed).

classify_font(
  element(font, Attrs, Children),
  element(font, [class=Style | Attrs], Children)
) :-
  \+ member(class=_, Attrs),
  member(joined, Attrs),
  member(name = FontName, Attrs),
  font_name_style(FontName, Style).
