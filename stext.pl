:- module(stext, [stext/0]).


% TODO remove this clause
stext :-
  !,
  stext_from_testfile.

stext :-
  user_input(Input),
  stext(Input).




stext(Input) :-
  load_xml(stream(Input), Xml, []),
  transform(Xml, XmlOut),
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
       document(Out)) :-
  once(member(element(A, B, C), Children)),
  transform([element(A,B,C)], Out).

% page-element
myrule(element(page, _A, Children),
       page(Out)) :-
  transform(Children, Out).

% line-element
myrule(element(line, A, Children),
       line(bbox(Bbox), Out)) :-
  transform(Children, Out),
  member(bbox=Bbox, A).

% block-element
myrule(element(block, _A, Children),
       block(Out)) :-
  transform(Children, Out).


% char
myrule(element(char, A, _C),
       c(Char)) :-
  member(c = Char, A).

myrule(Atom, removed) :- atom(Atom).


%
% End Transformation Rules
%

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
  stext_from_file("samples/hello.txt").


stext_from_file(FileName) :-
  with_open_file(FileName, read, [type(binary)], stext:stext).


with_open_file(FileName, Mode, Options, Goal) :-
  setup_call_cleanup(
    open(FileName, Mode, Stream, Options),
    call(Goal, Stream),
    close(Stream)
  ).


user_input(Input) :-
  stream_property(Input, alias(user_input)).



bbox_x(Bbox, X) :-
  split_string(Bbox, [' '], [], [First | _]),
  read_term_from_atom(First, X, []),
  number(X).

:- begin_tests(stext).

test(bbox_x) :-
  bbox_x('126.67302 676.25869 485.3213 688.40359',
        126.67302).


:- end_tests(stext).
