%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Meta-Interpretation from LMatrices
%	Author: S.H. Muggleton
%	Date: 4th August, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpq)).
:- [setarith,utils].

% :- expects_dialect(sicstus).
% :- use_module(library(timeout),[time_out/3]).

% To run:
% $ swipl
% ?- [interpret,compile,setarith,brackets].

% ?- lminterprets(Subs).

:- multifile maxcertdepth/1, hypmetasubs/2.
maxtime(0.2).

lminterprets(Subs) :- 	% Find Non-minimised solution
	current_predicate(nogen/0), !,
	time(lmcompile([pos,neg],MaxDepth,MaxLen,_)), pos(Pos), neg(Neg),
	writes(['Find Upperbound on Clauses','\n']),
	testlen(Pos,Neg,MaxLen,MaxDepth,Subs1), !,
	% length(Subs1,Max1),
	rulesnauxs(Subs1,Max1),
	writes(['Upperbound Clause Length is ',Max1,'\n','\n']),
	sort(Subs1,Subs), savehyp(Subs,MaxDepth), !.
lminterprets(Subs) :- 	% Find Minimised solution
	not(current_predicate(nogen/0)), !,
	time(lmcompile([pos,neg],MaxDepth,MaxLen,Min)), pos(Pos), neg(Neg),
	% writes(['Lower bound clauses length: ',Min,'\n']),
	writes(['Find Upperbound Clause Length','\n']),
	testlen(Pos,Neg,MaxLen,MaxDepth,Subs2), !,
	% length(Subs2,Max1),
	rulesnauxs(Subs2,Max1),
	% writes(['======================','\n']),
	% writes(['MOST SPECIFIC HYPOTHESIS [',Max1,' clauses]','\n','\n']),
	% pprog(Subs2),
	% writes(['======================','\n','\n']),
	% find_min_len(Pos,Neg,MaxDepth,1,Max1,Subs2,Subs1,MinLen),
	((current_predicate(minimum/1),minimum(Min1),!);Min1=Min),
	find_min_len(Pos,Neg,MaxDepth,Min1,Max1,Subs2,Subs1,MinLen),
	writes(['Minimal Program Length is ',MinLen,'\n','\n']),
	sort(Subs1,Subs), savehyp(Subs,MaxDepth), !.

% testlen - test whether consistent hypothesis contructible with
%	given maximum clause length Max

testlen(Pos,Neg,MaxLen,MaxDepth,Subs) :-
	T0 is cputime, % writes(['T0=',T0,'\n']),
	testlen0(T0,Pos,Neg,MaxLen,MaxDepth,Subs).

testlen0(T0,Pos,Neg,MaxLen,MaxDepth,Subs) :-
	writes(['======================','\n']),
	writes(['---------- Try ',MaxLen,' clauses: ']),
	proves(Pos,T0,[],Subs,MaxLen,MaxDepth,[],I),
	nproves(Neg,T0,Subs,I,MaxDepth),
	T1 is cputime-T0, decpl(T1,5,T2), writes(['Succeeded ',T2,'s','\n']),
	rulesnauxs(Subs,Max),
	writes(['CONSISTENT HYPOTHESIS [',Max,' clauses]','\n','\n']),
	% pprog(Subs,false).
	pprog(Subs,true,GR),
	% writes(['======================','\n','\n']),
	((pos([_]), GR > 0.03, writes(['** Failed since |E+|=1 and g(H)',' = ',GR,
		' > 0.03','\n','\n']),!,fail); true).
testlen0(T0,_,_,_,_,_) :-
	T1 is cputime-T0, decpl(T1,5,T2), writes(['Failed ',T2,'s','\n']),
	!, fail.

% find_min_len - find the minimum clause length hypothesis constructible
%	using binary chop search

find_min_len(_,_,_,Min,Min,Subs,Subs,Min1) :-
	rulesnauxs(Subs,Min1), !.	 % length(Subs,Min1), !.
				% Upper and lower bounds converge
find_min_len(Pos,Neg,MaxDepth,Lo,Hi,_,Subs3,MinDepth) :-
	Mid is Lo+(Hi-Lo)//2,
	testlen(Pos,Neg,Mid,MaxDepth,Subs2),
	find_min_len(Pos,Neg,MaxDepth,Lo,Mid,Subs2,Subs3,MinDepth), !.
find_min_len(Pos,Neg,MaxDepth,Lo,Hi,Subs1,Subs2,MinDepth) :-
	Mid is 1+Lo+(Hi-Lo)//2,
	find_min_len(Pos,Neg,MaxDepth,Mid,Hi,Subs1,Subs2,MinDepth), !.

proves(_,T0,_,_,_,_,_,_) :-
	maxtime(Tmax), T1 is cputime-T0,
	T1>=Tmax, !, fail.
proves([],_,Subs1,Subs2,_,_,I1,I2) :- reverse(Subs1,Subs2), reverse(I1,I2).
proves([Pos|T],T0,Subs1,Subs2,MaxLen,MaxDepth,I1,I2) :-
	Pos=..[P,X,Y],
	check_not_negative(P,X,Y),
	find_min_depth(up,X,Y,1,MaxDepth,MinDepth),
	% writes(['Max Depth: ',MaxDepth,'\n']),
	abduce_i(P/2,I1,I3,MaxLen),
	prove_d1(P,X,Y,T0,Subs1,Subs3,I3,I4,MaxLen,MaxDepth,MinDepth),
	proves(T,T0,Subs3,Subs2,MaxLen,MaxDepth,I4,I2).

nproves(_,T0,_,_,_) :-
	maxtime(Tmax), T1 is cputime-T0,
	T1>=Tmax, !, fail.
nproves([],_,_,_,_).
nproves([Neg|T],T0,Subs,I,MaxDepth) :-
	Neg=..[P,X,Y],
	find_min_depth(up,X,Y,1,MaxDepth,MinDepth),
	element(P/2,I),
	not((interval(MinDepth,MaxDepth,Interval), element(Depth,Interval),
		dprove(P,X,Y,Subs,I,Depth))),
	nproves(T,T0,Subs,I,MaxDepth).

% find_min_depth - find the minimum proof length of example up(X,Y)
%	using binary chop search

find_min_depth(_,_,_,Min,Min,Min) :- !.	% Upper and lower bounds converge
find_min_depth(Up,X,Y,Lo,Hi,MinDepth) :-
	Mid is Lo+(Hi-Lo)//2,
	find(Up,X,Y,Mid),
	find_min_depth(Up,X,Y,Lo,Mid,MinDepth), !.
find_min_depth(Up,X,Y,Lo,Hi,MinDepth) :-
	Mid is 1+Lo+(Hi-Lo)//2,
	find_min_depth(Up,X,Y,Mid,Hi,MinDepth), !.

% find/4 - find if the example is present in the diagonal union matrix
%	at the given depth

find(P,X,Y,Depth) :-
	mname(P,'1U',P1U), mname(P1U,Depth,Pu),
	% name(P,P1), name('1U',U1), name(Depth,D),
	% appends([P1,U1],Pu1), name(Pu0,Pu1), !,	% Pu0 = 'PU'
	% appends([Pu1,D],Pu2), name(Pu,Pu2), !,	% Pu = 'PUDepth'
	check_compiled(Pu,P1U,Depth), !,
	% (current_predicate(Pu,_) -> true;
		% otherwise -> lmconstruct(Pu0,Depth,_)), !,
	bit_call(Pu,X,Y), !.

% check_compiled/3 - Construct predicate if not already compiled

check_compiled(Pu,_,_) :- current_predicate(Pu,_), !.
check_compiled(_,Pu0,Depth) :- lmconstruct(Pu0,Depth,_), !.

% lminterpret1(Up,MinDepth,X,Y,Subs,Top) - generate meta-substitution Subs for
%	example (X,Y) and return name of Top program P

lminterpret1(_,1,X,Y,[s(P1,P)],P1) :-
	% mname(Up,1,Mname1),
	primitives1(Ps),
	element(P/2,Ps),
	call(P,X,Y), mname(P,1,P1), !.
lminterpret1(_,2,X,Y,[s(PQ,P,Q)],PQ) :-
	% mname(Up,2,Mname1),
	primitives1(Ps),
	element(P/2,Ps), element(Q/2,Ps),
	call(P,X,Z), call(Q,Z,Y), mnames([P,'_',Q],PQ), !.
lminterpret1(Up,Depth,X,Y,Subs,PQ) :-
	mname(Up,Depth,MnameD),
	check_compiled(MnameD,Up,Depth), !,
	bit_call(MnameD,X,Y),
	Depth2 is Depth//2, Depth3 is Depth-Depth2,
	mname(Up,Depth2,MnameD2), mname(Up,Depth3,MnameD3),
	check_compiled(MnameD2,Up,Depth2),
	bit_call(MnameD2,X,Z),
	check_compiled(MnameD3,Up,Depth3),
	bit_call(MnameD3,Z,Y),
	lminterpret1(Up,Depth2,X,Z,SubsXZ,P),
	lminterpret1(Up,Depth3,Z,Y,SubsZY,Q), mnames([P,'_',Q],PQ),
	set_unis([SubsXZ,SubsZY,[s(PQ,P,Q)]],Subs), !.

% list/1 - ensure list end with [].

list([]).
list([_|T]) :- list(T).

% pprog/2 - print the program

pprog(Prog,ShowEA,GR) :-	
	nauxs(Prog,Auxs,N,Prog1),
	defns(Prog1,Prog2),		% Partition rule set into definitions
	% writes(['HERE 1','\n']),
	% writes(['Prog = ',Prog,'\n']),
	length(Prog,N0),
	% writes(['Prog2=',Prog2,'\n']),
	% writes(['HERE 1a','\n']),
	evalg(Prog2,GQ,P0),			% Evaluate the g(H) function
					% using linear constraint solving
	% writes(['HERE 2','\n']),
	Prog1 = [[P/_,_]|_],		% P is topmost predicate
	% writes(['HERE 2a','\n']),
	% writes(['GQ=',GQ,'\n']),
	convQtR(GQ,GR),
	% writes(['HERE 3','\n']),
	% GS is ceiling(-log(GR)/log(2)),	% Surprisal in bits
	pos(Pos), length(Pos,M),
	% writes(['HERE 4','\n']),
	EE is  2*GR*(2.33+2*log(M))/M,
	EA is  100*(1-EE),
	decpl(EA,2,EA1),
	PH is 1/P0^(3*N0),
	NLpH is -log(PH),
	decpl(NLpH,2,NLpH1),
	NLpEH is -log(1/GR^M),
	decpl(NLpEH,2,NLpEH1),
	PHE is PH/GR^M,
	NLpHE is -log(PHE),
	decpl(NLpHE,2,NLpHE1),
	% DeltaPH is P0^(3*(N0+1))/P0^(3*N0),
	writes(['% g(',P,') = ',GR,' [',GQ,']','\n']),
	% writes(['HERE 5','\n']),
	% writes(['% p(',P,') = ',PH,'\n']),
	% writes(['% p(',P,'|E) = ',PHE,'\n']),
	writes(['% -ln p(',P,'|E) = ',NLpH1,NLpEH1,' = ',NLpHE1,'\n']),
	% writes(['% d p(',P,')/d n = ',DeltaPH,'\n']),
	% writes(['% Surprisal(g(',P,')) =< ',GS,' bits','\n']),
	(((ShowEA = true) -> writes(['% EA(',M,',',P,') >= ',EA1,'\n','\n']));nl),
	% writes(['% EA(',M,',',P,') > ',EA,'%','\n','\n']),
	pdefns(Prog2),
	writes(['% AUXILIARY DEFINITIONS [',N,']','\n','\n']),
	pprogaux(Auxs), !.

% convQtR/2 - convert a rational to a real number

convQtR(Rational,Real) :-
	% writes(['ConvQtR Rational=',Rational,'\n']),
	name(Rational,Chars),
	\+ member(114,Chars),
	Real is 0.999999*Rational, !.
convQtR(Rational,Real) :-
	name(Rational,Chars),
	append(Pre,[114|Post],Chars),
	name(Num,Pre),	% Numerator
	name(Den,Post),	% Denominator
	Real is Num/Den, !.

% evalg/2 - Evaluate the g(H) function using linear constraint solving

evalg(Prog,G,P0) :-			% Evaluate the g(H) function
	Prog = [[[TopP/_,_]|_]|_],
	% writes([Prog,'\n']),
 	% writes(['HERE EVALG 1','\n']),
	findall(P,member([[P/_,_]|_],Prog),HeadPs),
	findall(Q,(member(Dfn,Prog),member([_/_,Qs],Dfn),member(Q,Qs)),BodyPs),
	% writes(['HERE EVALG 2','\n']),
	% writes(['Prog = ',Prog,'\n']),
	% writes(['BodyPs = ',BodyPs,'\n']),
	Ps is_set HeadPs \/ BodyPs,
	length(Ps,P0),
	findall(R/_,member(R,Ps),PVars),
	% writes(['HERE EVALG 3','\n']),
	Prims is_set BodyPs -- HeadPs,
	% writes(['HeadPs = ',HeadPs,'\n']),
	% writes(['BodyPs = ',BodyPs,'\n']),
	% writes(['Prims = ',Prims,'\n']),
	length(Prims,NPrims), NPrims1 is NPrims+1, % Laplace correction
	% writes(['HERE EVALG 4','\n']),
	lineqns(Prog,PVars,NPrims1,Eqns), LinConstraint=..[{}|[Eqns]],
	% writes(['HERE EVALG 5','\n']),
	% writes(['LinConstraints = ',LinConstraint,'\n']),
	call(LinConstraint),
	% writes(['HERE EVALG 6','\n']),
	member(TopP/G,PVars), !.

% lineqns/4 - construct one linear equation for each definition

lineqns([Dfn],PVars,NPrims,Eqn) :-
	lineqn(Dfn,PVars,NPrims,Eqn), !.
lineqns([Dfn|Dfns],PVars,NPrims,(Eqn,Eqns)) :-
	lineqn(Dfn,PVars,NPrims,Eqn),
	lineqns(Dfns,PVars,NPrims,Eqns), !.

% lineqn/4 - construct a linear equation for one definition

lineqn(Dfn,PVars,NPrims,(V = Sum)) :-
	lneqn1(Dfn,PVars,NPrims,Prds),	% Make list of products
	mksum(Prds,Sum),			% Make into a sum
	Dfn = [[P/_|_]|_],
	member(P/V,PVars), !.

lneqn1([],_,_,[]) :- !.
lneqn1([C|Cs],PVars,NPrims,[Eqn|Eqns]) :-
	lneqn2(C,PVars,NPrims,Eqn),
	lneqn1(Cs,PVars,NPrims,Eqns), !.

lneqn2([_/r1,[_]|_],_,NPrims,(1/NPrims)) :- !.
lneqn2([_/r2,[_,_]|_],_,NPrims,(1/NPrims^2)) :- !.
lneqn2([_/r3,[_,Q]|_],PVars,NPrims,(V/NPrims)) :-
	member(Q/V,PVars), !.

% lneqn2([_/r1,[_]|_],_,NPrims,(2/(NPrims+1))) :- !.	% Laplace correction
% lneqn2([_/r2,[_,_]|_],_,NPrims,(2/(NPrims+1)^2)) :- !.
% lneqn2([_/r3,[_,Q]|_],PVars,NPrims,((V+1)/(NPrims+1))) :-
%	member(Q/V,PVars), !.

% mksum/2 - transform a list of products into a sum of the products

mksum([Prd],Prd) :- !.
mksum([Prd1|Prds],(Prd1+Prd2)) :-
	mksum(Prds,Prd2), !.

% defns/2 - Partition rule set into definitions

defns([],[]) :- !.
defns(Prog,[Def|Defs]) :-
	Prog = [[P/_,_]|_],
	defn(P,Prog,Def,Rest),
	defns(Rest,Defs), !.

% defn/2 - Extract one definition

defn(_,[],[],[]) :- !.
defn(P,[H|T1],[H|T2],Rest) :-
	H = [P/_,_],
	defn(P,T1,T2,Rest), !.
defn(P,[H|T],[],[H|T]) :-
	H \= [P/_,_], !.

% nauxs - find the total number of clauses in the auxiliary definitions
%	associated with the given meta-substitution.

nauxs(Prog,Auxs1,N,Prog2) :- 
% nauxs(Prog,_,N,Prog2) :- 
	current_predicate(auxdef/2),
	convrules(Prog,Prog1), sort(Prog1,Prog2),
	findall(P/Qs,(element([_/_,Ps],Prog2),
		element(P,Ps),auxdef(P,Qs)),Auxs),
	sort(Auxs,Auxs1), nauxs1(Auxs1,N), !.
	% sort(Auxs,_), N=0, !.
nauxs(Prog,[],0,Prog2) :- 
	convrules(Prog,Prog1), sort(Prog1,Prog2), !.

nauxs1([],0) :- !.
nauxs1([_/Qs|T],N) :-
	nauxs1(T,M),
	length(Qs,Qn), N is M+(Qn-1), !.	% Assumes no overlap
						% in sub-definitions
rulesnauxs(G,RnAuxs) :-
	length(G,N), nauxs(G,_,M,_), RnAuxs is N+M, !.

% pprogaux/2 - print auxiliary definitions

pprogaux([]) :- !.
pprogaux([P/[Q,R]|Rest]) :- 			% Pair
	writes([P,'(X,Y) :- ',Q,'(X,Z), ',R,'(Z,Y).','\n']),
	pprogaux(Rest).
pprogaux([P/[Q,R,S,T]|Rest]) :- 		% Quad
	writes([P,'(X,Y) :- ',Q,R,'(X,Z), ',S,T,'(Z,Y).','\n']),
	mnames([Q,R],QR), mnames([S,T],ST),
	pprogaux([QR/[Q,R]]), pprogaux([ST/[S,T]]),
	pprogaux(Rest).
pprogaux([P/[Q,R,S,T,U,V,W,X]|Rest]) :-		% Octup
	writes([P,'(X,Y) :- ',Q,R,S,T,'(X,Z), ',U,V,W,X,'(Z,Y).','\n']),
	mnames([Q,R,S,T],QRST), mnames([U,V,W,X],UVWX),
	pprogaux([QRST/[Q,R,S,T]]), pprogaux([UVWX/[U,V,W,X]]),
	pprogaux(Rest).
pprogaux([Q/[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]|Rest]) :-	% Sixteen
	writes([Q,'(X,Y) :- ',A,B,C,D,E,F,G,H,'(X,Z), ',I,J,K,L,M,N,O,P,'(Z,Y).','\n']),
	mnames([A,B,C,D,E,F,G,H],ABCDEFGH), mnames([I,J,K,L,M,N,O,P],IJKLMNOP),
	pprogaux([ABCDEFGH/[A,B,C,D,E,F,G,H]]), 
	pprogaux([IJKLMNOP/[I,J,K,L,M,N,O,P]]),
	pprogaux(Rest).

% convrules - convert a meta-substitution into pairings [P,Qs] where
%	P and Qs are the predicates in the Head and Body of the rules.

convrules([],[]) :- !.
convrules([rule1_d1(P,Q)|Subs],[[P/r1,[Q]]|Subs1]) :- !,
	convrules(Subs,Subs1), !.
convrules([rule2_d1(P,Q,R)|Subs],[[P/r2,[Q,R]]|Subs1]) :- !,
	convrules(Subs,Subs1), !.
convrules([rule3_d1(P,Q,R)|Subs],[[P/r3,[Q,R]]|Subs1]) :- !,
	convrules(Subs,Subs1), !.

% pdefns/1 - print each definition separated by a newline

pdefns([]) :- !.
pdefns([Defn|Defns]) :-
	pdefn(Defn),
	pdefns(Defns), !.

% pdefn/1 - print one defintion

pdefn([]) :- nl, !.
pdefn([[P/r1,[Q]]|Subs]) :-
	writes([P,'(X,Y) :- ',Q,'(X,Y).','\n']),
	pdefn(Subs).
pdefn([[P/r2,[Q,R]]|Subs]) :-
	writes([P,'(X,Y) :- ',Q,'(X,Z), ',R,'(Z,Y).','\n']),
	pdefn(Subs), !.
pdefn([[P/r3,[Q,R]]|Subs]) :-
	writes([P,'(X,Y) :- ',Q,'(X,Z), ',R,'(Z,Y).','\n']),
	pdefn(Subs), !.

% prove/3 - Metagol general meta-interpreter

% MetagolV1 rules

prove_d1(P,X,Y,_,S,S,I,I,_,MaxDepth,_) :-
	reverse(S,S1),
	dprove(P,X,Y,S1,I,MaxDepth), !.	% Deductive derivation of example
prove_d1(P,X,Y,T0,S1,S2,I1,I2,MaxLen,MaxDepth,MinDepth) :-
	interval(MinDepth,MaxDepth,Interval), element(Depth,Interval),
	% writes([prove_d1(P,X,Y,T0,S1,S2,I1,I2,MaxLen,MaxDepth,MinDepth),Depth,'\n']),
	iprove(P,X,Y,T0,S1,S2,I1,I2,MaxLen,MaxDepth,Depth).
					% Inductive derivation of example

% iprove/10 - inductive derivation of example given Metarules

% iprove(P,X,Y,T0,S1,S2,I,I,MaxLen,MaxDepth,1) :-
%	primitives1(Ps),			% P(x,y) <- Q(x,y) % IDENT
%	element(Q/2,Ps), call(Q,X,Y),
%	abduce(rule1_d1(P,Q),S1,S2,MaxLen),
%	neg(Negs), nproves(Negs,_,S2,I,MaxDepth).
% iprove(P,X,Y,T0,S1,S2,I,I,MaxLen,MaxDepth,Depth) :- Depth=<2,
iprove(P,X,Y,T0,S1,S2,I,I,MaxLen,MaxDepth,Depth) :- Depth=<2,	% Base case
	primitives1(Ps),			% P(x,y) <- Q(x,z), R(z,y)
	element(Q/2,Ps), call(Q,X,Z),
        % Depth1 is Depth-1,
        % find_min_depth(up,Z,Y,1,Depth1,Right),
        % mname(up,Right,MnameD), % Check solution at Depth Right
        % check_compiled(MnameD,up,Right),
	% bit_call(MnameD,Z,Y),
	element(R/2,Ps), call(R,Z,Y),
	abduce(rule2_d1(P,Q,R),S1,S2,MaxLen),
	neg(Negs), nproves(Negs,T0,S2,I,MaxDepth), !.
iprove(P,X,Y,T0,S1,S2,I1,I2,MaxLen,MaxDepth,Depth) :- Depth > 1,
                                                % P(x,y) <- Q(x,z), P(z,y)
                                                % CHAIN/TAILREC
        (
                (primitives1(Ps),element(Q/2,Ps),call(Q,X,Z))
                % ;
                % (element(Q/2,I1),Q\=P,dprove(Q,X,Z,S1,I1,1))
        ),
        Depth1 is Depth-1,
        % find_min_depth(up,X,Y,0,Depth1,Right),
        find_min_depth(up,Z,Y,1,Depth1,Right),
	% writes([Q,X,Z,Right,'\n']),
        mname(up,Right,MnameD), % Check solution at Depth Right
        check_compiled(MnameD,up,Right),
	bit_call(MnameD,Z,Y),
        ((newpred(P,I1,MaxLen,R));element(R/2,I1)), abduce_i(R/2,I1,I5,MaxLen),
		% mnames([P,'_',Right],R)
        abduce(rule3_d1(P,Q,R),S1,S4,MaxLen),
        check_not_negative(R,Z,Y),
        neg(Negs), nproves(Negs,T0,S4,I5,MaxDepth), % !,
        iprove(R,Z,Y,T0,S4,S2,I5,I2,MaxLen,MaxDepth,Right). % Right decomposition
% iprove(P,X,Y,T0,S1,S2,I1,I2,MaxLen,MaxDepth,Depth) :- Depth > 1, % !,
% 						% P(x,y) <- P(x,z), Q(z,y)
% 						% CHAIN/LEFTREC
% 	current_predicate(cf/0),
% 	Left is Depth-1,
% 	mname(up,Left,MnameD), % Check solution at Depth Left
% 	check_compiled(MnameD,up,Left),
% 	bit_call(MnameD,X,Z),
% 	((element(Q/2,I1),Q\=P);(mnames([P,'_',Left],Q))),
% 	abduce(Q/2,I1,I3,MaxLen),
% 	(
% 		(primitives1(Ps),element(R/2,Ps),call(R,Z,Y))
% 		;
% 		(element(R/2,I1),R\=P,dprove(R,Z,Y,S1,I1,1))
% 	),
% 	abduce(rule4_d1(P,Q,R),S1,S3,MaxLen),
% 	check_not_negative(Q,X,Z),
% 	neg(Negs), nproves(Negs,T0,S3,I3,MaxDepth),
% 	iprove(Q,X,Z,T0,S3,S2,I3,I2,MaxLen,MaxDepth,Left). % Left decomposition

% newpred - create a new predicate symbol P_N, distinct from 
%	those in the predicate set I

newpred(P,I,Max,Q) :-
	interval(1,Max,Ints), element(N,Ints),
	mnames([P,'_',N],Q), not(element(Q,I)), !.

% dprove/6 - deductive derivation of example given Metarules

% dprove(P,X,Y,S,_,1) :-
%	element(rule1_d1(P,Q),S), 	% P(x,y) <- Q(x,y) IDENT
%	primitives1(Ps), element(Q/2,Ps),	%  where Q,R primitives
%	call(Q,X,Y).		
dprove(P,X,Y,S,_,Depth) :- Depth =< 2,	% P(x,y) <- Q(x,z), R(z,y)
	element(rule2_d1(P,Q,R),S), 	
	primitives1(Ps),			
	element(Q/2,Ps), call(Q,X,Z),
	element(R/2,Ps), call(R,Z,Y).
dprove(P,X,Y,S,I,Depth) :- Depth > 2,
	element(rule3_d1(P,Q,R),S),
	(
		(primitives1(Ps),element(Q/2,Ps),call(Q,X,Z))
	),
	element(R/2,I),
	Depth1 is Depth-1,
	dprove(R,Z,Y,S,I,Depth1).		% Right decomposition
						% P(x,y) <- Q(x,z), P(z,y)
						% CHAIN/TAILREC
% dprove(P,X,Y,S,I,Depth) :- Depth > 1,
% 	element(rule4_d1(P,Q,R),S),
% 	Depth1 is Depth-1,
% 	% find_min_depth(up,X,Y,1,Depth,Left1), Left is Left1-1,
% 	element(Q/2,I),
% 	dprove(Q,X,Z,S,I,Depth1),		% Left decomposition
% 	(
% 		(element(R/2,I),R\=P,dprove(R,Z,Y,S,I,1))
% 		;
% 		(primitives(Ps),element(R/2,Ps),call(R,Z,Y))
% 	).
						% P(x,y) <- Q(x,z), P(z,y)
						% CHAIN/TAILREC
% Check consistence of goal with negatives - TEST INOCORATION

check_not_negative(P,X,Y) :- 
	Goal =.. [P,X,Y], neg(Negs),
	\+ element(Goal,Negs).

abduce_i(X,I1,I2,MaxLen) :- abduce1(X,I1,I2), !,
	length(I2,N),
	N=<MaxLen.
abduce_i(X,I1,I2,_) :- abduce1(X,I1,I2).	% Add predicate

% abduce/3 - Check if X is in list, and add to the head otherwise.

abduce(X,G1,G2,MaxLen) :- abduce1(X,G1,G2), !, % pprog(G2), !,
	length(G2,N),
	nauxs(G2,_,M,_), % writes(['N,M are ',N,',',M,'\n','\n']),
	% countaux(G2,M),
	Cs is N+M, Cs=<MaxLen.
	% Replace length by countaux
abduce(X,G1,G2,_) :- abduce1(X,G1,G2).	% Add predicate

abduce1(X,G,G) :- element(X,G).
abduce1(X,G,[X|G]) :- not(element(X,G)).

% countauxs/2 - finds how many distinct auxiliary predicates are
%	used in the meta-substitution G

countaux(G,N) :- findall(P,(element(rule3_d1(_,P,_),G),auxdef(P,_)),Auxs),
	sort(Auxs,Auxs1), length(Auxs1,N), !.
	% writes(['\n','\n','Auxs = ',Auxs1,'\n']).

% interval/3 - find a list of numbers representing the interval [Lo,..,Hi]

interval(Lo,Hi,[Lo|T]) :-
        Lo=<Hi, Lo1 is Lo+1,
        interval(Lo1,Hi,T), !.
interval(_,_,[]).

% savehyp/2 - print the hypothesis and print to the screen

savehyp(Subs,_) :-
	% length(Subs,N),
	rulesnauxs(Subs,N),
	writes(['======================','\n']),
	writes(['% HYPOTHESISED PROGRAM (',N,' clauses)','\n','\n']),
	pprog(Subs,true,_),
	writes(['======================','\n','\n']),
	tell('hypothesis.pl'),
	writes(['% To run:','\n','% $ swipl','\n',
		'% ?- [interpret,compile,setarith,lib_dl,primitives1,hypothesis,modes].','\n','\n']),
	writes(['% HYPOTHESISED PROGRAM (',N,' clauses)','\n','\n']),
	pprog(Subs,true,_),
	findall(P/2,(element(Sub,Subs),Sub=..[_,P|_]),I), sort(I,I1),
	writes(['\n','% Hypothesis Meta-Substitution:','\n','hypmetasubs(',Subs,',',I1,').','\n']),
	% writes(['\n','% Max certificate depth:','\n','maxcertdepth(',MaxDepth,').','\n']),
	% writes(['\n','% Max certificate depth:','\n','maxcertdepth(',64,').','\n']),
	told, !.
