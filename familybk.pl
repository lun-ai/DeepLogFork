%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Family Background Knowledge
%	Author: S.H. Muggleton
%	Date: 27th July, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

parent(harry,john). parent(harry,tom). parent(sally,john). parent(sally,tom).
parent(john,maggie). parent(john,jeremy). parent(edith,maggie). parent(edith,jeremy).
parent(tom,phil). parent(tom,alice). parent(mary,phil). parent(mary,alice).
parent(bill,jill). parent(bill,bob). parent(maggie,jill). parent(maggie,bob).
parent(bob,henry). parent(henry,will). parent(will,fred).

child(X,Y) :- parent(Y,X).
