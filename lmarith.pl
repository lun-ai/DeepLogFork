%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Logical Matrix arithmetic package
%	Author: S.H. Muggleton
%	Date: 27th July, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [setarith,utils].
:- ['bmlp/swipl_client.pl'].

:- op(700,xfy,is_lmatrix). 		% LMatrix expression evaluation
:- op(500,yfx,'--').			% LMatrix minus intersection

M is_lmatrix A^2 :- !,
	Ma is_lmatrix A,
	lm_square(Ma,M), !.
M is_lmatrix A \/ 1 :- !,
	Ma is_lmatrix A,
	lm_add_diagonal(Ma,M), !.
M is_lmatrix A * B :- !,
	Ma is_lmatrix A,
	Mb is_lmatrix B,
	!, lm_prod(Ma,Mb,M), !.
M is_lmatrix M :- !, nonvar(M), !.

% Square of lmatrices

lm_square([P,M1],[P,M2]) :- M2 is M1*2,
	mfname(P,M2,M2name),
	bmlp_gpu_sq([P,M1],[P,M2]), 
	reconsult(M2name), !.
	% lm_prod([P,M1],[P,M1],[P,M2]), !.

% lm_prod/3 - Ms is productof matrices M1,M2

lm_prod([P,M1],[P,M2],[P,M3]) :- M3 is M1+M2,
	mfname(P,M3,M3name),
	% tell(M3name),
	% pprod([P,M1],[P,M2],[P,M3]),
	% told,
	bmlp_gpu_mul([P,M1],[P,M2],[P,M3]),
	reconsult(M3name), !.
	% lm_prod_trans([P,M3]), !.

% lm_prod_trans/1 - Generation of Transpose Matrix

lm_prod_trans([P,M]) :-
	mname(t,P,TP), mfname(TP,M,Mname),
	tell(Mname),
	mname(P,M,P_M), mname(TP,M,TP_M),
	writes(['% Transposed Union Matrix','\n']),
	call(P_M,Y,_),
	findall(X,(call(P_M,X,Ys),1 is getbit(Ys,Y)),Xs),
	lm_stob1(Xs,BXs),
	writes([TP_M,'(',Y,',',BXs,').','\n']), fail.
lm_prod_trans([P,M]) :- !,
	told,
	mname(t,P,TP), mfname(TP,M,Mname),
	reconsult(Mname).

% lm_prods of list of lmatrices

lm_prods(P,[Depth],[P,Depth]) :- !.
lm_prods(P,[D|Ds],M) :-
	lm_prods(P,Ds,M1),
	M is_lmatrix [P,D] * M1.
%	lm_prod([P,D],M1,M).


% Perform the matrix power operation as a SWI-Prolog client call to Python BMLP-GPU library
bmlp_gpu_sq([P,N1],[P,N2]) :-
	run_python_command("import bmlp.matrix", _),
	mfname(P,N1,Pfname1),
	format(
		string(C1),
		"a = bmlp.matrix.integers_to_boolean_matrix('~w')",
		[Pfname1]),
	run_python_command(C1, _Res1),
	format(
		string(C2),
		"b = a @ a",
		[]),
	run_python_command(C2, _Res2),
	(var(N2) -> N2 is N1*2; true),
	mfname(P,N2,Pfname2),
	mname(P,N2,Pname2),
	format(
		string(C3),
		"bmlp.matrix.boolean_matrix_to_integers(b,'~w','~w')",
		[Pname2,Pfname2]),
	run_python_command(C3, _Res3).


% Perform the matrix power operation as a SWI-Prolog client call to Python BMLP-GPU library
bmlp_gpu_mul([P,N1],[P,N2],[P,N3]) :-
	run_python_command("import bmlp.matrix", _),
	% Load the first matrix
	mfname(P,N1,Pfname1),
	format(
		string(C1),
		"a = bmlp.matrix.integers_to_boolean_matrix('~w')",
		[Pfname1]),
	run_python_command(C1, _Res1),
	% Load the second matrix
	mfname(P,N2,Pfname2),
	format(
		string(C2),
		"b = bmlp.matrix.integers_to_boolean_matrix('~w')",
		[Pfname2]),
	run_python_command(C2, _Res2),
	% Multiply the first matrix and the second matrix
	format(
		string(C3),
		"c = a @ b",
		[]),
	run_python_command(C3, _Res3),
	% Update the matrix Id integer to be the sum of Ids of two matrices
	(var(N3) -> N3 is N1 + N2; true),
	mfname(P,N3,Pfname3),
	mname(P,N3,Pname3),
	format(
		string(C4),
		"bmlp.matrix.boolean_matrix_to_integers(c,'~w','~w')",
		[Pname3,Pfname3]),
	run_python_command(C4, _Res4).


% Printing product of lmatrices

pprod([P,M1],[P,M2],[P,M3]) :-
	mname(t,P,TP),  % Name of Transpose Matrix
	mname(P,M1,P_M1), mname(TP,M2,TP_M2), mname(P,M3,P_M3),
	% mname(TP,M3,TP_M3),
	call(P_M1,X,BXs),
	findall(Y,(call(TP_M2,Y,BYs),BZs is BXs/\BYs,BZs>0),Ys1), 
	lm_stob1(Ys1,BYs1),
	writes([P_M3,'(',X,',',BYs1,').','\n']), fail.
pprod(_,_,_).

% Add diagonal to a given logical matrix

lm_add_diagonal([P,M],[Pu,M]) :-
	name(P,P1), name('1U',U1), 
	appends([P1,U1],Pu1), name(Pu,Pu1),
	mfname(Pu,M,Mfname),
	tell(Mfname),
	pdiagonal([P,M],[Pu,M]),
	told,
	reconsult(Mfname), !.

% Printing Matrix with diagonal added

pdiagonal([P,M],[Pu1,M]) :-
	writes(['% Matrix with diagonal added','\n']),
	mname(P,M,P_M), mname(Pu1,M,Pu1_M),
	call(P_M,X,_), lm_add_diagonal1(P_M,X,Y),
	writes([Pu1_M,'(',X,',',Y,')','.','\n']), fail.
% pdiagonal([P,M],[Pu1,M]) :-
% 	writes(['% Transpose Matrix with diagonal added','\n']),
% 	mname(P,M,P_M), mname(Pu1,M,Pu1_M),
% 	mname(t,P_M,TP_M), mname(t,Pu1_M,TPu1_M),
% 	call(TP_M,X,_), lm_add_diagonal1(TP_M,X,Y),
% 	writes([TPu1_M,'(',X,',',Y,')','.','\n']), fail.
pdiagonal(_,_).

% Add_diagonal into a row of the matrix

lm_add_diagonal1(P_M,X,Y) :-
	call(P_M,X,Y1), Y is Y1\/1<<X, !.
	

element(X,[X|_]).
element(X,[_|T]) :- element(X,T).

mfname(P,M,Fname) :-
	name(P,P1), name(M,M1), name('.pl',DotPl),
	appends([P1,M1,DotPl],Fname1),
	name(Fname,Fname1), !.

mname(P,M,Mname) :-
	name(P,P1), name(M,M1),
	appends([P1,M1],Mname1),
	name(Mname,Mname1), !.

mnames([Name],Name) :- !.
mnames([H|T],Name) :-
	mnames(T,NameT),
	mname(H,NameT,Name), !.

% lm_submatrix/2 - test whether every row of 1st matrix is a subset
%	of the corresponding row of the second
%

lm_submatrix([P,M1],[P,M2]) :-
	mname(P,M1,PM1), mname(P,M2,PM2),
	\+((call(PM1,X,Y1),call(PM2,X,Y2),\+(bit_subset(Y1,Y2)))).


% lm_term2const/2 - convert a term to a constant and vice versa

lm_term2const(Term,Const) :- var(Term), var(Const), !, fail.
lm_term2const(Term,Const) :- nonvar(Term), term_to_atom(Term,Const), !.
lm_term2const(Term,Const) :- nonvar(Const), atom_to_term(Const,Term,[]).

% lm_mkcton/1 - make one-one mapping from Herbrand Base to Natural numbers
%	and create auxiliary primitives based on frequent pairings.

lm_mkcton(Cs) :-
	File= 'herbn.pl',
	tell(File),
	writes([':- discontiguous(cton/2), discontiguous(ntoc/2).','\n','\n']),
	lm_mkcton(Cs,0),
	told, reconsult(File), !.

% lm_mkauxps/2 - print the auxiliary high-frequency invented predicates

lm_mkauxps([],[],[]) :- writes(['\n','\n']), !.
lm_mkauxps([[P1,P2]|AuxPairs],[P1P2|AuxPs],
		[[P1P2,[P1,P2]]|AuxDefs]) :-			% Pair
	writes(['\n',P1P2,'(X,Y) :- ',
		P1,'(X,Z), ',P2,'(Z,Y).','\n']),
	lm_mkauxps(AuxPairs,AuxPs,AuxDefs), !.
lm_mkauxps([[P1,P2,P3,P4]|AuxTups],[P1P2P3P4|AuxPs],
		[[P1P2P3P4,[P1,P2,P3,P4]]|AuxDefs]) :-		% Quad
 	writes(['\n',P1P2P3P4,'(X,Y) :- ',
 		P1,P2,'(X,Z),',P3,P4,'(Z,Y).','\n']),
 	lm_mkauxps(AuxTups,AuxPs,AuxDefs), !.
lm_mkauxps([[P1,P2,P3,P4,P5,P6,P7,P8]|AuxTups],[P1P2P3P4P5P6P7P8|AuxPs],
 		[[P1P2P3P4P5P6P7P8,[P1,P2,P3,P4,P5,P6,P7,P8]]|AuxDefs]) :-
  								% Octuple
  	writes(['\n',P1P2P3P4P5P6P7P8,'(X,Y) :- ',
  		P1,P2,P3,P4,'(X,Z),',P5,P6,P7,P8,'(Z,Y).','\n']),
  	lm_mkauxps(AuxTups,AuxPs,AuxDefs), !.
lm_mkauxps([[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16]|AuxTups],
	[P1P2P3P4P5P6P7P8P9P10P11P12P13P14P15P16|AuxPs],
	[[P1P2P3P4P5P6P7P8P9P10P11P12P13P14P15P16,
	[P1,P2,P3,P4,P5,P6,P7,P8,
	 P9,P10,P11,P12,P13,P14,P15,P16]]|AuxDefs]) :-
  								% Sixteen
  	writes(['\n',P1P2P3P4P5P6P7P8P9P10P11P12P13P14P15P16,'(X,Y) :- ',
  		P1,P2,P3,P4,P5,P6,P7,P8,'(X,Z),',
		P9,P10,P11,P12,P13,P14,P15,P16,'(Z,Y).','\n']),
  	lm_mkauxps(AuxTups,AuxPs,AuxDefs), !.

lm_mkcton([],_) :- !.
lm_mkcton([H|T],N) :-
	portray_clause(cton(H,N)), write(' '),
	portray_clause(ntoc(N,H)), nl,
	% writes(['cton(',H,',',N,'). ',
	%	'ntoc(',N,',',H,').','\n']),
	N1 is N+1,
	lm_mkcton(T,N1), !.

% lm_stob/2 - convert a subset of the Herbrand Base to a Bitset

lm_stob(Set,Bitset) :-
	lm_stob(Set,0,Bitset).

lm_stob([],Bs,Bs) :- !.
lm_stob([H|T],Bs1,Bs2) :-
	cton(H,N),
	Bs3 is Bs1\/1<<N,
	lm_stob(T,Bs3,Bs2), !.

% lm_stob1/2 - convert a set of Numbers to a Bitset

lm_stob1(NSet,Bitset) :-
	lm_stob1(NSet,0,Bitset).

lm_stob1([],Bs,Bs) :- !.
lm_stob1([N|T],Bs1,Bs2) :-
	Bs3 is Bs1\/1<<N,
	lm_stob1(T,Bs3,Bs2), !.

% lm_btos/2 - convert a Bitset to a subset of the Herbrand Base

lm_btos(Bitset,Set) :-
	lm_btos(Bitset,[],Set).

lm_btos(0,Set,Set) :- !.
lm_btos(Bs1,Set1,[H|Set2]) :-
	N is lsb(Bs1), ntoc(N,H),
	Bs2 is Bs1/\ \(1<<N),
	lm_btos(Bs2,Set1,Set2), !.
