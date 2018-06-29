:- module(font, [font_rule/3]).


% tarpit.pdf
font_rule(name='CMTI10', Children, italic(Children)).
font_rule(name='CMBX12', Children, heading(2, Children)).

% halmos.pdf
font_rule(name='CMBX10', Children, heading(1, Children)).
font_rule(name='CMCSC10', Children, heading(2, Children)).
font_rule(name='CMR8', Children, subtitle(Children)).
font_rule(name='CMR10', Children, body(Children)).
font_rule(name='CMR7', Children, footer(Children)).
