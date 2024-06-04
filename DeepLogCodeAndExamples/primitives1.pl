% User and Auxiliary [1 High-Frequency Primitives]

primitives1([eq/2,aa/2,a/2,b/2]).


aa(X,Y) :- a(X,Z), a(Z,Y).


auxdef(aa,[a,a]). 

