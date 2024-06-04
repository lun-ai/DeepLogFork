% To run:
% $ swipl
% ?- [interpret,compile,setarith,lib_dl,primitives1,hypothesis,modes].

% HYPOTHESISED PROGRAM (9 clauses)

% g(regex) = 0.10008 [1251r12500]
% -ln p(regex|E) = 52.74-9.2 = 43.53
% EA(4,regex) >= 74.47

regex(X,Y) :- b(X,Z), a(Z,Y).
regex(X,Y) :- eq(X,Z), b(Z,Y).
regex(X,Y) :- a(X,Z), regex(Z,Y).
regex(X,Y) :- aa(X,Z), regex_1(Z,Y).

regex_1(X,Y) :- a(X,Z), regex_1_1(Z,Y).

regex_1_1(X,Y) :- b(X,Z), regex_1_1_1(Z,Y).

regex_1_1_1(X,Y) :- aa(X,Z), regex_1_1_1_1(Z,Y).

regex_1_1_1_1(X,Y) :- eq(X,Z), aa(Z,Y).

% AUXILIARY DEFINITIONS [1]

aa(X,Y) :- a(X,Z), a(Z,Y).

% Hypothesis Meta-Substitution:
hypmetasubs([rule2_d1(regex,b,a),rule2_d1(regex,eq,b),rule2_d1(regex_1_1_1_1,eq,aa),rule3_d1(regex,a,regex),rule3_d1(regex,aa,regex_1),rule3_d1(regex_1,a,regex_1_1),rule3_d1(regex_1_1,b,regex_1_1_1),rule3_d1(regex_1_1_1,aa,regex_1_1_1_1)],[regex/2,regex_1/2,regex_1_1/2,regex_1_1_1/2,regex_1_1_1_1/2]).
