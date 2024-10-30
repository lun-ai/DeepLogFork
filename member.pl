%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Member of a list
%	Author: S.H. Muggleton
%	Date: 5th January, 2022
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

pos([ member([aa,ff,gg],aa),
	member([aa,bb,cc,dd,ee],dd)]).
neg([]).

testpos([member([aa,bb,cc],aa),member([bb,cc,aa],cc)]).
testneg([member([aa,bb,cc,dd],ee)]).
