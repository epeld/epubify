:- module(font,
          [
            font_rule/3,
            font_name_style/2
          ]).


% tarpit.pdf
font_rule(name='CMTI10', Children, italic(Children)).
font_rule(name='CMBX12', Children, heading(2, Children)).

% halmos.pdf
font_rule(name='CMBX10', Children, heading(1, Children)).
font_rule(name='CMCSC10', Children, heading(2, Children)).
font_rule(name='CMR8', Children, subtitle(Children)).
font_rule(name='CMR10', Children, body(Children)).
font_rule(name='CMR7', Children, footer(Children)).




font_name_style('CMTI10', italic).
font_name_style('CMBX12', heading(2)).

% halmos.pdf
font_name_style('CMBX10', heading(1)).
font_name_style('CMCSC10', heading(2)).
font_name_style('CMR8', subtitle).
font_name_style('CMR10', regular).
font_name_style('CMR7', footer).
