%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Reverse and uppercase a list of characters
%	Author: S.H. Muggleton
%	Date: 27th June, 2022
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

% aux.

pos([revupc([a,l,i,c,e|cl],['E','C','I','L','A'|cl])]).
% pos([revupc([h,a,r,r,y|cl],['Y','R','R','A','H'|cl])]).
% pos([revupc([m,a,r,y|cl],['Y','R','A','M'|cl])]).
% pos([revupc([t,o,m|cl],['m','o','t'|cl])]).
neg([]).

testpos([
	revupc([b,o,b|cl],['B','O','B'|cl]),
	revupc([h,a,r,r,y|cl],['Y','R','R','A','H'|cl]),
	revupc([m,a,r,y|cl],['Y','R','A','M'|cl]),
	revupc([t,o,m|cl],['M','O','T'|cl])
]).
testneg([
	revupc([a|cl],[b|cl]),
	revupc([a,l,i,c,e|cl],['A','L','I','C','E'|cl])
]).
