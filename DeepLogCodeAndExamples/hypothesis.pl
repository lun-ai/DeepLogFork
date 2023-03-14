% To run:
% $ swipl
% ?- [interpret,compile,setarith,lib_dl,primitives1,hypothesis,modes].

% HYPOTHESISED PROGRAM (7 clauses)

% g(abcs04) = 0.000925925925925926 [1r1080]
% -ln p(abcs04|E) = 24.96-6.98 = 17.97
% EA(1,abcs04) >= 99.57

abcs04(X,Y) :- a(X,Z), abcs04_1(Z,Y).

abcs04_1(X,Y) :- b(X,Z), abcs04_1_1(Z,Y).

abcs04_1_1(X,Y) :- g(X,Z), h(Z,Y).
abcs04_1_1(X,Y) :- cdef(X,Z), abcs04_1_1(Z,Y).

% AUXILIARY DEFINITIONS [3]

cdef(X,Y) :- cd(X,Z), ef(Z,Y).
cd(X,Y) :- c(X,Z), d(Z,Y).
ef(X,Y) :- e(X,Z), f(Z,Y).

% Hypothesis Meta-Substitution:
hypmetasubs([rule2_d1(abcs04_1_1,g,h),rule3_d1(abcs04,a,abcs04_1),rule3_d1(abcs04_1,b,abcs04_1_1),rule3_d1(abcs04_1_1,cdef,abcs04_1_1)],[abcs04/2,abcs04_1/2,abcs04_1_1/2]).
