:- module(stext, []).


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
rule(
  line(_A,
       [ font([name='CMBX10',size='9.963'],
              Text)
       ]),
  heading(1, Text)
).

rule(
  line(_A,
       [font([name='CMR8',size='7.97'], Text)]),
  subtitle(Text)
).

rule(
  line(_A,
       [ font([name='CMR10',size='9.963'], _Number),
         font([name='CMCSC10',size='9.963'], Text)
       ]),
  heading(2, Text)
).

rule(
  line(_A,
       [ font([name='CMR10',size='9.963'],
              Text)
       ]),
  body(line(Text))
).

%
% End Transformation Rules
%

apply_rules([], []).
apply_rules([Item | Xml], [ItemOut | XmlOut]) :-
  rule(Item, ItemOut).


transform(Xml, XmlOut) :-
  member(element(document, _Attrs, Children),
         Xml),
  include(is_page, Children, Pages),
  maplist(pretty_page, Pages, PagesOut),
  XmlOut = PagesOut.


pretty_page(element(page, _A, Children),
            page(ChildrenOut)) :-
  include(is_element, Children, Children0),
  maplist(pretty_block, Children0, ChildrenOut).


pretty_block(element(block, _A, Children),
             block(ChildrenOut)) :-
  include(is_element, Children, Children0),
  maplist(pretty_line, Children0, ChildrenOut).


pretty_line(element(line, A, Children),
            line(bbox(BBox), ChildrenOut)) :-
  member(bbox=BBox, A),
  include(is_element, Children, Children0),
  maplist(pretty_font, Children0, ChildrenOut).

pretty_font(element(font, A, Children),
            font(A, ChildrenOut)) :-
  % Children = ChildrenOut.
  include(is_element, Children, Children0),
  maplist(pretty_char, Children0, Chars),
  maplist(string_codes, Chars, Codes),
  append(Codes, ChildrenOut0),
  string_codes(ChildrenOut, ChildrenOut0)
.

pretty_char(element(char, Attrs, _C), Char) :-
  member((c = Char), Attrs).

is_element(element(_A,_B,_C)).

pretty_element(element(Type, _A, Children),
               Functor) :-
  functor(Functor, Type, 1),
  arg(1, Functor, Children).

is_page(element(page, _A, _C)).


write_commands(Commands, Stream) :-
  %format(Stream, "~w", [Commands]).
  print_term(Commands, [output(Stream)]).

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
