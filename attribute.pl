%
% This module contains utilities
% for dealing with (elements and) attributes
%
:- module(attribute,
          [
            attribute_tag/3,
            element_attribute/2,
            element_children/2,
            element_tag/2
          ]).

element_tag(element(El, _1, _2), El).

% Yeah this predicate doesn't really belong here
element_children(element(_1, _2, C), C).

%
% Util for tagging attributes
%
attribute_tag(Tag, A, [Tag | A]) :-
  \+ member(Tag, A).

%
% Unifies with the element's attributes
element_attribute(element(_T, A, _C), Attribute) :-
  member(Attribute, A).
