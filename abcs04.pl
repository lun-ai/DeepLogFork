%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	ab(cd)cl(efg)clhi grammar example
%	Author: S.H. Muggleton
%	Date: 16th September, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

aux.

pos([
	abcs04([a,b,c,d,e,f,c,d,e,f,g,h|cl],cl)
]).
neg([]).

testpos([
	abcs04([a,b,c,d,e,f,c,d,e,f,g,h|cl],cl)
]).
testneg([
	abcs04([f,f,e,a,h,g,c,d,d,c,e,b|cl],cl),
	abcs04([e,d,f,a,c,c,e,g,h,b,d,f|cl],cl),
	abcs04([g,c,a,f,d,e,b,d,h,c,f,e|cl],cl),
	abcs04([f,c,a,f,e,d,b,c,h,g,e,d|cl],cl),
	abcs04([g,f,b,h,a,f,e,d,c,d,e,c|cl],cl),
	abcs04([d,f,c,e,h,b,c,f,g,a,e,d|cl],cl),
	abcs04([e,e,c,g,f,h,a,d,f,c,d,b|cl],cl),
	abcs04([f,d,g,e,f,c,h,d,a,c,b,e|cl],cl),
	abcs04([b,e,g,d,h,a,f,c,d,e,c,f|cl],cl),
	abcs04([g,a,b,c,e,d,d,f,h,e,c,f|cl],cl),
	abcs04([c,e,e,a,b,f,f,c,g,d,d,h|cl],cl),
	abcs04([e,d,f,f,g,c,b,e,h,c,d,a|cl],cl),
	abcs04([e,b,e,g,c,a,h,f,d,f,c,d|cl],cl),
	abcs04([e,d,g,b,f,c,a,d,e,f,c,h|cl],cl),
	abcs04([c,a,g,d,h,f,e,e,b,c,d,f|cl],cl),
	abcs04([e,c,f,d,a,g,h,d,f,c,e,b|cl],cl),
	abcs04([d,a,f,e,c,f,e,c,h,b,d,g|cl],cl)
]).
