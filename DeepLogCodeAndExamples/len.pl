%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Length of a list
%	Author: S.H. Muggleton
%	Date: 27th June, 2022
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

aux.

pos([lengthlist([5,3,2,1|nl],4)]).
neg([]).

testpos([lengthlist([1,2,3,4|nl],4)]).
testneg([lengthlist([2,3,1|nl],1)]).
