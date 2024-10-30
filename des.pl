%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Ancestor example
%	Author: S.H. Muggleton
%	Date: 8th February, 2023
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

% aux.

pos([
	descendent(will,henry),
	descendent(fred,harry)
]).
neg([]).

testpos([
	descendent(fred,john),
	descendent(fred,edith),
	descendent(will,john)
]).
testneg([
	descendent(john,fred)
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

