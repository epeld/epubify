%
% This module defines the transformation rule 'myrule'
% that was used initially for testing
%
:- module(myrule, [myrule/2]).
:- use_module(font, [font_rule/3]).
:- use_module(bbox, [bbox_x/2]).
:- use_module(transform, [transform/3]).


%
% Transformation Rules
%

% TODO reconsider this rule?
% font_rule invocation
myrule(element(font, A, Text), Out) :-
  font_rule(Name, Out0, Out),
  member(Name, A),
  transform(myrule, Text, Out0).


myrule(element(page, _A, Children),
       element(semantic_page, [], Out)) :-
  maplist(semantic_elements, Children, Elements),
  append(Elements, Out).


myrule(element(document, _A, Children),
       element(semantic_document, [], Out)) :-
  maplist(=(element(semantic_page, _1, _2)), Children),
  maplist(element_children, Children, All),
  append(All, Out).


% block-element (after converting to body_lines)
myrule(element(block, _A, Children),
       element(block, [], Out)) :-
  member(element(body_line, _1, _2), Children),
  body_paragraphs(Children, Out).


myrule(El, Out) :-
  xml_rule(El, Out).

%
% 'Semantic' rules
%
myrule(element(line, A, Text),
       element(heading, [n=N | A], C3)) :-
  member(heading(N, C), Text),
  stringified(C, C3).

myrule(element(line, A, Text),
       element(subtitle, A, C2)) :-
  member(subtitle(C), Text),
  stringified(C, C2).

myrule(element(line, A, Text),
       element(body_line, A, C2)) :-
  member(body(C), Text),
  stringified(C, C2).

myrule(element(block, _1, C),
       removed) :-
  member(element(line, _2, C2), C),
  member(footer(_3), C2).


%
% Transformation Helpers
%
semantic(element(_1, _2, Children)) :-
  foreach(member(C, Children),
          (
            string(C)
          )
         ).

semantic_elements(element(block, [], Children),
                  Children) :-
  foreach(member(C, Children),
          semantic(C)).

element_children(element(_1, _2, Children), Children).


body_paragraphs(T, P) :- body_paragraphs(T, [], P).

body_paragraphs([], P, PR) :- reverse(P, PR).

body_paragraphs([ element(body_line, [ P ], [ S ]) | Rest],
                Paragraphs,
                Out) :-
  member(P, [ indented, aligned_differently ]),
  body_paragraphs(Rest, [element(paragraph, [], [ S ]) | Paragraphs], Out).


body_paragraphs([ element(body_line, [ none ], [ S ]) | Rest],
                [],
                Out) :-
  body_paragraphs(Rest, [element(paragraph, [continued], [S])], Out).

body_paragraphs([ element(body_line, [ none ], [ S ]) | Rest],
                [ element(paragraph, A, [SPrev]) | Paragraphs],
                Out) :-
  string_concat(SPrev, " ", SNext0),
  string_concat(SNext0, S, SNext),
  body_paragraphs(Rest, [element(paragraph, A, [SNext]) | Paragraphs], Out).


stringified(C, S) :-
  maplist(to_code, C, Codes),
  append(Codes, Codes2),
  string_codes(C2, Codes2),
  split_string(C2, [], " ", S).

to_code(c(S), C) :- string_codes(S, C).


%
%
%



% document-element
xml_rule(element(document, _A, Children),
       element(document, [], Out)) :-
  % once(member(element(A, B, C), Children)),
  transform(myrule, Children, Out).

% page-element
xml_rule(element(page, _A, Children),
       element(page, [], Out)) :-
  transform(myrule, Children, Out).

% line-element
xml_rule(element(line, A, Children),
       element(line, [Indentation], Out)) :-
  transform(myrule, Children, Out),
  member(bbox=Bbox, A),
  bbox_x(Bbox, X),
  classify_indentation(X, Indentation).

% block-element
xml_rule(element(block, _A, Children),
       element(block, [], Out)) :-
  transform(myrule, Children, Out).

% char
xml_rule(element(char, A, _C),
       c(Char)) :-
  member(c = Char, A).

% Remove e.g '\n'
xml_rule(Atom, removed) :- atom(Atom).


%
% Helper predicates
%

classify_indentation(X, none) :- X < 138.
classify_indentation(X, indented) :- 138 =< X, X < 140.
classify_indentation(X, aligned_differently) :- 140 =< X.
