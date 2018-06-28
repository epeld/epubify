:- module(stext, [stext/0]).

with_open_file(FileName, Mode, Options, Goal) :-
  setup_call_cleanup(
    open(FileName, Mode, Stream, Options),
    call(Goal, Stream),
    close(Stream)
  ).


stext :-
  stext_from_testfile.



stext(Input) :-
  load_xml(stream(Input), Xml, []),
  format("Transforming..~n"),
  transform(Xml, XmlOut),
  format("Done..~n"),
  !,
  with_open_file("/tmp/out.txt", write, [],
                 write_commands(XmlOut)),

  !.


%
% Transformation Rules
%

font_rule([name='CMBX10',size='9.963'], Children, heading(1, Children)).
font_rule([name='CMCSC10',size='9.963'], Children, heading(2, Children)).
font_rule([name='CMR8',size='7.97'], Children, subtitle(Children)).
font_rule([name='CMR10', size='9.963'], Children, body(Children)).
font_rule([name='CMR7',size='6.974'], Children, footer(Children)).

% font_rule invocation
myrule(element(font, A, Text), Out) :-
  font_rule(A, Out0, Out),
  transform(Text, Out0).


% document-element
myrule(element(document, _A, Children),
       element(document, [], Out)) :-
  % once(member(element(A, B, C), Children)),
  transform(Children, Out).

myrule(element(document, _A, Children),
       element(semantic_document, [], Out)) :-
  maplist(=(element(semantic_page, _1, _2)), Children),
  maplist(element_children, Children, All),
  append(All, Out).

% page-element
myrule(element(page, _A, Children),
       element(page, [], Out)) :-
  transform(Children, Out).

myrule(element(page, _A, Children),
       element(semantic_page, [], Out)) :-
  maplist(semantic_elements, Children, Elements),
  append(Elements, Out).

% line-element
myrule(element(line, A, Children),
       element(line, [Indentation], Out)) :-
  transform(Children, Out),
  member(bbox=Bbox, A),
  bbox_x(Bbox, X),
  classify_indentation(X, Indentation).

% block-element
myrule(element(block, _A, Children),
       element(block, [], Out)) :-
  transform(Children, Out).

% block-element (after converting to body_lines)
myrule(element(block, _A, Children),
       element(block, [], Out)) :-
  member(element(body_line, _1, _2), Children),
  body_paragraphs(Children, Out).

% char
myrule(element(char, A, _C),
       c(Char)) :-
  member(c = Char, A).

% Remove e.g '\n'
myrule(Atom, removed) :- atom(Atom).

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
% End Transformation Rules
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

%wrap_paragraph(Item, element(paragraph, [], [Item])).
body_paragraphs(T, P) :- body_paragraphs(T, [], P).

body_paragraphs([], P, PR) :- reverse(P, PR).

body_paragraphs([ element(body_line, [ P ], [ S ]) | Rest],
                Paragraphs,
                Out) :-
  member(P, [ indented, aligned_differently ]),
  body_paragraphs(Rest, [element(paragraph, [], [ S ]) | Paragraphs], Out).

%% body_paragraphs([ element(body_line, [ aligned_differently ], [ S ]) | Rest],
%%                 Paragraphs,
%%                 Out) :-
%%   body_paragraphs(Rest, [element(paragraph, [], [ S ]) | Paragraphs], Out).

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
% Perform transformation rules recursively
% until no more transformations apply
apply_rule(Element, Out) :-
  (myrule(Element, Out0), Out0 \= Element) *->
    (
      Out = success(Out0)
    )
  ; failure(Element) = Out.


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
  
get_element(success(R), R).
get_element(failure(R), R).


write_commands(Commands, Stream) :-
  %format(Stream, "~w", [Commands]).
  print_term(
    Commands,
    [
      output(Stream),
      right_margin(60),
      % tab_width(2),
      indent_arguments(true)
    ]
  ).

stext_from_testfile :-
  stext_from_file("~/Downloads/tarpit.pdf").


stext_from_file(FileName) :-
  setup_call_cleanup(
    mutool_draw(FileName, all, Stream),
    stext(Stream),
    close(Stream)
  ).


mutool_draw(FileName, Page, StdOut) :-
  mutool_draw_args(FileName, Page, Args),
  process_create(
    path(mutool),
    Args,
    [
      stdout(pipe(StdOut))
    ]
  ).


mutool_draw_args(FileName, all, Args) :-
  Args = [
    'draw',
    '-X',
    '-Fstext',
    FileName
  ].

mutool_draw_args(FileName, Page, Args) :-
  number(Page),
  Args = [
    'draw',
    '-X',
    '-Fstext',
    FileName
  ].



classify_indentation(X, none) :- X < 138.
classify_indentation(X, indented) :- 138 =< X, X < 140.
classify_indentation(X, aligned_differently) :- 140 =< X.

bbox_x(Bbox, X) :-
  split_string(Bbox, [' '], [], [First | _]),
  read_term_from_atom(First, X, []),
  number(X).

:- begin_tests(stext).

test(bbox_x) :-
  bbox_x('126.67302 676.25869 485.3213 688.40359',
        126.67302).


:- end_tests(stext).
