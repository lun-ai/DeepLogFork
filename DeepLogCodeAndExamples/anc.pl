%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Ancestor example
%	Author: S.H. Muggleton
%	Date: 27th July, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

% aux.

pos([
	ancestor(henry,will),
	ancestor(harry,fred)
]).
neg([]).

testpos([
	ancestor(john,fred),
	ancestor(edith,fred),
	ancestor(john,will)
]).
testneg([
	ancestor(fred,john)
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 	Family Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			harry+sally
%	   	/		\
%		john+edith		tom+mary
%		/	\		/	\
%	bill+maggie	jeremy		phil	alice
%	/	\
%	jill	bob+sarah
%       	\
%     		henry+millie
%		\
%		will+joanne
%		\
%		fred+edith

