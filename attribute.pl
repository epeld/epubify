:- module(attribute, [attribute_tag/3]).


%
% Util for tagging attributes
%
attribute_tag(Tag, A, [Tag | A]) :-
  \+ member(Tag, A).
