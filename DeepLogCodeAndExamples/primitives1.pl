% User and Auxiliary [4 High-Frequency Primitives]

primitives1([eq/2,ef/2,de/2,cdef/2,cd/2,a/2,b/2,c/2,d/2,e/2,f/2,g/2,h/2]).


cd(X,Y) :- c(X,Z), d(Z,Y).

cdef(X,Y) :- cd(X,Z),ef(Z,Y).

de(X,Y) :- d(X,Z), e(Z,Y).

ef(X,Y) :- e(X,Z), f(Z,Y).


auxdef(cd,[c,d]). auxdef(cdef,[c,d,e,f]). auxdef(de,[d,e]). auxdef(ef,[e,f]). 

