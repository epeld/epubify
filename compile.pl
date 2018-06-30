:- module(compile, []).
:- use_module(load, []).
:- use_module(stext, [stext/0]).


%
% Produce the prologpdf command line tool.
% Note that the new "prologpdf"-process
%
% Will be invoked with the same command line
% flags as your current prolog process
%
compile_app :-
  qsave_program(
    "epubify",
    [
      goal(
        (
          stext,
          halt
        )
      ),
      stand_alone(false)
    ]
  ).
