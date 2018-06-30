:- module(bbox,
          [
            bbox_x/2,
            element_bbox_x/2
          ]).
:- use_module(attribute, [element_attribute/2]).


element_bbox_x(El, X) :-
  element_attribute(El, bbox=Bbox),
  bbox_x(Bbox, X).

% Given a bbox-attribute, parse out the x-coordiante
bbox_x(Bbox, X) :-
  split_string(Bbox, [' '], [], [First | _]),
  read_term_from_atom(First, X, []),
  number(X).

:- begin_tests(bbox).

test(bbox_x) :-
  bbox_x('126.67302 676.25869 485.3213 688.40359',
        126.67302).


:- end_tests(bbox).
