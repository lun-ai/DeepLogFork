%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	ab(cd)+(efg)+hi grammar example
%	Author: S.H. Muggleton
%	Date: 16th September, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

aux.

pos([abcs08([a,b,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
	z,z|cl],cl)]).
neg([]).

testpos([abcs08([a,b,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
	  z,z|cl],cl),
	abcs08([a,b,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
	  z,z|cl],cl),
	abcs08([a,b,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
		c,d,e,f, g,h,i,j,
	  z,z|cl],cl)
]).
testneg([abcs08([a|cl],cl)]).
