:- module(transform, [transform/2]).
:- use_module(xml, [xml_rule/2]).
:- use_module(font, [font_rule/3]).

transform(Xml, Out) :- 
  maplist(apply_rule, Xml, Out0),
  exclude(=(success(removed)),Out0, Out1),
  maplist(get_element, Out1, Out2),
  (
    (
      member(success(_), Out1)
    )*-> (!, transform(Out2, Out))
  ; Out2 = Out
  ).


%
% Perform transformation rules recursively
% until no more transformations apply
apply_rule(Element, Out) :-
  (myrule(Element, Out0), Out0 \= Element) *->
    (
      Out = success(Out0)
    )
  ; failure(Element) = Out.

 
get_element(success(R), R).
get_element(failure(R), R).


%
% Transformation Rules
%

% TODO reconsider this rule?
% font_rule invocation
myrule(element(font, A, Text), Out) :-
  font_rule(Name, Out0, Out),
  member(Name, A),
  transform(Text, Out0).


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


