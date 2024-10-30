%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Par3 examples
%	Author: S.H. Muggleton
%	Date: 27th July, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

pos([
	par3(sally,bob),
	par3(harry,bob)
]).
neg([]).

testpos([
	par3(sally,jill),
	par3(harry,bob)
]).
testneg([
	par3(fred,john),
	par3(john,fred)
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

