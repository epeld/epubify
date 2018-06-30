:- module(font,
          [
            font_rule/2,
            font_name_style/2
          ]).
:- use_module(attribute, [attribute_tag/3]).


% tarpit.pdf
deprecated_font_stuff(name='CMTI10', Children, italic(Children)).
deprecated_font_stuff(name='CMBX12', Children, heading(2, Children)).

% halmos.pdf
deprecated_font_stuff(name='CMBX10', Children, heading(1, Children)).
deprecated_font_stuff(name='CMCSC10', Children, heading(2, Children)).
deprecated_font_stuff(name='CMR8', Children, subtitle(Children)).
deprecated_font_stuff(name='CMR10', Children, body(Children)).
deprecated_font_stuff(name='CMR7', Children, footer(Children)).




font_name_style('CMTI10', italic).
font_name_style('CMBX12', heading(2)).

% halmos.pdf
font_name_style('CMBX10', heading(1)).
font_name_style('CMCSC10', heading(2)).
font_name_style('CMR8', subtitle).
font_name_style('CMR10', regular).
font_name_style('CMR7', footer).


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
  element(font, AttrsOut, Transformed)
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
