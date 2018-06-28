:- module(stext, [stext/2]).
:- use_module(transform, [transform/2]).
:- use_module(file, [with_open_file/4]).


stext(test, Page) :-
  stext_from_testfile(Page).

stext(file(FileName), Page) :-
  stext_from_file(FileName, Page).

stext(Input) :-
  load_xml(stream(Input), Xml, []),
  format("Transforming..~n"),
  transform(Xml, XmlOut),
  format("Done..~n"),
  !,
  with_open_file("/tmp/out.txt", write, [],
                 write_commands(XmlOut)),
  !,
  format("File Written~n").


stext_from_testfile(Page) :-
  stext_from_file("/home/erik/Downloads/out-of-the-tar-pit.pdf", Page).


stext_from_file(FileName, Page) :-
  setup_call_cleanup(
    mutool_draw(FileName, Page, Stream),
    stext(Stream),
    close(Stream)
  ).


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
    FileName,
    Page
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
