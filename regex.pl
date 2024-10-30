:- [lib_dl].		% Common background knowledge

aux.

pos([
	regex([a,a,a,b,a,a,a,a|cl],cl),
	regex([a,b,a|cl],cl),
	regex([b|cl],cl),
	regex([b,a|cl],cl)
]).
neg([]).

testpos([
]).
testneg([
]).
