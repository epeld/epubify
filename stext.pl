:- module(stext, [stext/2, stext/0]).
:- use_module(transform, [apply_singleton_rule/3]).
:- use_module(file, [with_open_file/4]).
:- use_module(document, [transformation/2]).

stext :-
  current_prolog_flag(argv, [FileName, Page]),
  stext(FileName, Page).

stext(test, Page) :-
  stext_from_testfile(Page).

stext(file(FileName), Page) :-
  stext_from_file(FileName, Page).

stext(Input) :-
  load_xml(stream(Input), [Document | _], []),
  format("Transforming..~n"),
  apply_singleton_rule(transformation, Document, XmlOut),
  format("Done..~n"),
  !,
  with_open_file("/tmp/out.txt", write, [],
                 stext:write_commands(XmlOut)),
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
  print_term(
    Commands,
    [
      output(Stream),
      right_margin(60),
      % tab_width(2),
      indent_arguments(true)
    ]
  ).



%
% Utils for Invoking 'mupdf'
%
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
