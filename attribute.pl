:- module(attribute,
          [
            attribute_tag/3,
            element_attribute/2
          ]).


%
% Util for tagging attributes
%
attribute_tag(Tag, A, [Tag | A]) :-
  \+ member(Tag, A).

%
% Unifies with the element's attributes
element_attribute(element(_T, A, _C), Attribute) :-
  member(Attribute, A).
