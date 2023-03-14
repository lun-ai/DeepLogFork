%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Compile Primitives into LMatrices
%	Author: S.H. Muggleton
%	Date: 27th July, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lmarith].

lmcompile(TrainTest,Depth,NCs,MinLen) :- 
	primitives(Ps),
	constants(TrainTest,Ps,Cs), MinLen=1, length(Cs,NCs),
	% primitives1(Ps1),
	writes(['Minimal Herbrand Base is ',NCs,'\n','\n']),
	% plUmatrices(Ps1,Cs),
	plUmatrices(Ps,Cs),
	sqp1fixedpoint(up,Depth),
	sqpconstruct([up,1],Depth).

% mkmatrix(Up,Depth,M) :- % Create primitive matrix as product
%	binarise(Depth,Depths),
%	mkmatrices(Up,Depths,M), !.

% mkmatrices(Up,[Depth],[Up,Depth]) :- !.
% mkmatrices(Up,[Depth|Depths],M) :-
%	mkmatrices(Up,Depths,M1),
%	M is_lmatrix [Up,Depth] * M1.

% clevel(2).
% clevel(20).
% clevel(20).
% clevel(32).
% clevel(64).
clevel(200).
% clevel(512).

% constants/2 - Find all constants used in the primitives and
%	augment the primitives with high-frequency auxiliary predicates.

constants([TP,TN],Ps,CsN) :-
	call(TP,Pos), call(TN,Neg),
	% pos(Pos), neg(Neg), testpos(TPos), testneg(TNeg),
	appends([Pos,Neg],PosNeg),
	findtypes(PosNeg),	% Argument types put into modes.pl
	constants1(PosNeg,Ps,[],CsN,[],FreqPss,[],PsCore),
	writes(['PsCore is ',PsCore,'\n']),
	lm_mkcton(CsN),
	findall(Ps1,(element(Pss1,FreqPss),Pss1\=[],	% Identify auxiliaries
		element(Ps1,Pss1)),FreqPs1),
	sort(FreqPs1,AuxPairs),
	% writes(['AuxPairs = ',AuxPairs,'\n','\n']),
	% findall(NamePs1/2,(element(Ps1,AuxPairs),mnames(Ps1,NamePs1)),AuxPs),
	findall(NamePs1/2,(element(Ps1,AuxPairs),mnames(Ps1,NamePs1)),AuxPs),
	sort(AuxPs,AuxPs0), reverse(AuxPs0,RevAuxPs),
	findall(NamePs2,(element(NamePs2/2,AuxPs0)),AuxPs1),
	writes(['\n','AuxPs1 = ',AuxPs1,'\n','\n']),
	((current_predicate(aux/0),AuxPs2=RevAuxPs);AuxPs2=[]),
	% appends([AuxPs2,PsCore,[eq/2]],PsAuxPs), tell('primitives1.pl'),
	appends([[eq/2],AuxPs2,PsCore],PsAuxPs), tell('primitives1.pl'),
		length(AuxPs2,NAux),
	writes(['% User and Auxiliary [',NAux,
		' High-Frequency Primitives]','\n','\n']),
	writes(['primitives1(',PsAuxPs,').','\n','\n']),
	lm_mkauxps(AuxPairs,AuxPs1,AuxDefs),
	% writes(['AuxDefs = ',AuxDefs,'\n','\n']),
	writeauxdefs(AuxDefs),
	% writeauxps(AuxPs1),
	% writes(['auxdefs(',AuxDefs,').','\n','\n']),
	told, reconsult(primitives1), !.

% writeadefs/1 - prints the set of auxiliary predicate symbols

writeauxdefs([]) :- writes(['\n','\n']), !.
writeauxdefs([[P,Qs]|AuxPs]) :-
	writes([auxdef(P,Qs),'.',' ']),
	writeauxdefs(AuxPs).


% csprune - prunes the primtive predicate cross-product Pss to only
%	those relating in 

csprune(_,_,[],U,U,Pss,Pss) :- !.
	% length(U,N),
	% writes(['\n','Herbrand Universe pruned to ',U,'\n',
		% 'of size ',N,'\n','\n']), !.
csprune(Ps,Ys,[Xs|T],U1,U2,Pss1,Pss2) :- % Prune the constants partition
				% from the example output end
	findall(X,(element(P/2,Ps),element(X,Xs),element(Y,Ys),
		call(P,X,Y)),Xs1),
	findall(P,(element(P/2,Ps),element(X,Xs),element(Y,Ys),
		call(P,X,Y)),Ps0),
	U3 is_set U1 \/ Xs1,
	sort(Ps0,Ps1),
	csprune(Ps,Xs1,T,U3,U2,[Ps1|Pss1],Pss2), !.

% constants1 - GIVEN <X,Y> instances, and primitives Ps
%	FIND
%	  U - minimal Herbrand universe
%	  FreqPss - frequencies of pairs <P,Q> of predicate
%		symbols in Pss (sequence of sets of predicates
%		which span the levelwise partition Css of constants in U)
%	  NPs - maximum number of distinct predicates in Pss

constants1([],_,U,U,FreqPss,FreqPss,PsCore,PsCore).
constants1([Pos|T],Ps,U1,U2,FreqPss1,[FreqPs|FreqPss2],PsCore1,PsCore2) :-
	Pos=..[_,X,Y],
	constants2(X,Y,Ps,0,U3,Pss),
	writes(['\n','Pss is ',Pss,'\n']),
	findall(P/2,(element(Ps3,Pss),element(P,Ps3)),PsC), sort(PsC,PsCore0),
	% writes(['HERE 1','\n']),
	PsCore3 is_set PsCore1 \/ PsCore0,
	findall([P2,Q2],(suffix(Pss,[Pp2,Qq2|_]),			% Pairs
		element(P2,Pp2),element(Q2,Qq2)),Ppairs),
	% writes(['HERE 2','\n']),
 	findall([P4,Q4,R4,S4],(suffix(Pss,[Ps4,Qs4,Rs4,Ss4|_]),	% Quads
 		element(P4,Ps4),element(Q4,Qs4),
 		element(R4,Rs4),element(S4,Ss4)),Pquads),
	% writes(['HERE 3','\n']),
 	findall([P8,Q8,R8,S8,T8,U8,V8,W8],
 		(suffix(Pss,[Ps8,Qs8,Rs8,Ss8,Ts8,Us8,Vs8,Ws8|_]),
 		element(P8,Ps8),element(Q8,Qs8),element(R8,Rs8),
 		element(S8,Ss8),element(T8,Ts8),element(U8,Us8),
 		element(V8,Vs8),element(W8,Ws8)),
		Poctups),					% Octups
	% writes(['HERE 4','\n']),
 	findall([A16,B16,C16,D16,E16,F16,G16,H16,
		 I16,J16,K16,L16,M16,N16,O16,P16],
 		(suffix(Pss,[As16,Bs16,Cs16,Ds16,Es16,Fs16,Gs16,Hs16,
			     Is16,Js16,Ks16,Ls16,Ms16,Ns16,Os16,Ps16|_]),
		element(A16,As16),element(B16,Bs16),element(C16,Cs16),
		element(D16,Ds16),element(E16,Es16),element(F16,Fs16),
		element(G16,Gs16),element(H16,Hs16),element(I16,Is16),
		element(J16,Js16),element(K16,Ks16),element(L16,Ls16),
		element(M16,Ms16),element(N16,Ns16),element(O16,Os16),
		element(P16,Ps16)),
		Psedecs),					% Sixteen
	% writes(['HERE 5','\n']),
	msort(Ppairs,PpSorted), msort(Pquads,PqSorted),
		msort(Poctups,PrSorted), msort(Psedecs,PsSorted),
	appends([PpSorted,PqSorted,PrSorted,PsSorted],Psorted),
	countbag(Psorted,Pcounts),
	% writes(['\n','Pcounts are ',Pcounts,'\n']),
	findall(P,(element(P^N,Pcounts),N>=2),FreqPs),
	% writes(['\n','FreqPs are ',FreqPs,'\n']),
	U4 is_set U1 \/ U3 \/ [Y],
	constants1(T,Ps,U4,U2,FreqPss1,FreqPss2,PsCore3,PsCore2), !.

% constants2 calls constants3 to finds the minimal certificate for deriving the output
%	Y from the input X, and then prunes the partition of constants
%	at successive depths using constants1, to identify the constants
%	and primitives used in minimal certificates.

constants2(X,Y,Ps,Depth,U,Pss) :-
	constants3(Y,Ps,Depth,[[X]],[_|Css],[],_),
	% writes(['\n','\n','constants3 gives Css as ',Css,'\n','\n']),
	csprune(Ps,[Y],Css,[],U,[],Pss), !.

% constants3 finds the minimal sequence for deriving the output Y from the input X
%	(found in Css1). This is done by finding successive blocks of
%	new constants in a partition. The depth of new blocks is
%	bounded by the clevel/1 value.

% constants3(Y,_,Depth,Css,Css,U,U) :-
constants3(Y,_,_,Css,Css,U,U) :-
	% writes(['U: ',U,'\n']),
	Css=[Cs|_], element(Y,Cs), !. % Found output Y
	% writes(['\n','Css is ',Css,'\n']),
	% writes(['\n','Found ',Y,' at depth ',Depth,' in','\n']),
	% writes([U,'\n']),
	% length(U,N),
	% writes(['Herbrand Universe has cardinality ',N,'\n','\n']), !.
% constants3(Y,_,Depth,Css,Css,U,U) :-		% Depth limit check
constants3(_,_,Depth,Css,Css,U,U) :-		% Depth limit check
	clevel(Max), Depth>Max, !.
	% writes(['\n','Reached depth limit of ',Max,'\n','\n']), !, fail.
	% writes(['\n','Failed to find ',Y,'at depth limit of ',Max,'\n','\n']), !.
constants3(Y,Ps,Depth,[Cs1|T],Css2,U1,U2) :-	% Add another layer
	findall(O,(element(P/2,Ps),element(I,Cs1),call(P,I,O)),Os),
	% writes(['Next layer: ',Os,'\n','\n']),
	Cs2 is_set Os -- U1,
	U3 is_set U1 \/ Cs2,
	Depth1 is Depth+1,
	constants3(Y,Ps,Depth1,[Cs2,Cs1|T],Css2,U3,U2), !.


% plmatrices/2 - Print the matrices for each of the primitives

plmatrices(Ps,Cs) :-
	element(P/2,Ps),
	mfname(P,1,Mname),
	tell(Mname),
	plmatrix(P,Cs),
	told,
	reconsult(Mname),
	fail.
plmatrices(_,_) :- !.

plmatrix(P,Cs) :-
	element(X,Cs),
	findall(Y,(call(P,X,Y)),Ys1), sort(Ys1,Ys),
	mname(P,1,P_1),
	writes([P_1,'(',X,',',Ys,').','\n']), fail.
plmatrix(_,_) :- !.

% plUmatrices/2 - Print the Union and Transposed Union of primitives matrices

plUmatrices(Ps,Cs) :-
	plUmatrices1(Ps,Cs),
	plUmatrices2(Ps,Cs), !.

plUmatrices1(Ps,Cs) :-	% Union matrix
	Up= 'up',
	mfname(Up,1,Mname),
	tell(Mname),
	mname(Up,1,Up_1),
	writes(['% Union Matrix','\n']),
	element(X,Cs),
	findall(Y,(element(P/2,Ps),call(P,X,Y),element(Y,Cs)),Ys),
	cton(X,BX), lm_stob(Ys,BYs),
	writes([Up_1,'(',BX,',',BYs,').','\n']), fail.
plUmatrices1(_,_) :- !,
	told,
	Up= 'up',
	mfname(Up,1,Mname),
	reconsult(Mname).

plUmatrices2(_,Cs) :-	% Transposed Union matrix
	TUp= 'tup',
	mfname(TUp,1,Mname),
	tell(Mname),
	mname(TUp,1,TUp_1),
	writes(['% Transposed Union Matrix','\n']),
	element(Y,Cs), cton(Y,BY),
	findall(X,(up1(X,Ys),1 is getbit(Ys,BY)),Xs),
	lm_stob1(Xs,BXs),
	writes([TUp_1,'(',BY,',',BXs,').','\n']), fail.
plUmatrices2(_,_) :- !,
	told,
	TUp= 'tup',
	mfname(TUp,1,Mname),
	reconsult(Mname).

% sqp1fixedpoint/2 - print the square of matrices U1 to the fixed point

sqp1fixedpoint(P,Depth) :-	% First find depth of fixed point
	M1 is_lmatrix [P,1] \/ 1,
	fixedpoint(M1,1,Depth),
	writes(['Fixed point is ',Depth,'\n']).

fixedpoint(M1,N1,Depth) :-
	M2 is_lmatrix M1^2,
	\+(lm_submatrix(M2,M1)), N2 is N1*2,
	% writes([M1,' is a proper subset of ',M2,'\n']),
	fixedpoint(M2,N2,Depth), !.
fixedpoint(_,Depth,Depth) :-	% Reached fixed point
	% writes(['Fixed point matrix is ',M,'\n']),
	!.

% Compute squares of matrices without diagonal up to 2^Depth

sqpconstruct([M1,N1],Depth) :-
	N1 =< Depth,
	[M2,N2] is_lmatrix [M1,N1]^2,
	sqpconstruct([M2,N2],Depth), !.
sqpconstruct(_,_) :- !.

% binarise/2 - breaks the given number N into a list of powers of 2
%	which sums to N.

binarise(N,Pow2s) :- binarise(N,1,Pow2s).

binarise(0,_,[]).
binarise(N,X,[X|T]) :-	% N is odd
	1 is N/\1, !,
	X1 is X<<1,
	N1 is N>>1,
	binarise(N1,X1,T).
binarise(N,X,T) :-	% N is even
	X1 is X<<1,
	N1 is N>>1,
	binarise(N1,X1,T).

% lmconstruct - construct a new lmatrix which as a product of powers-of-2
%	lmatrices.

lmconstruct(Up,N,M) :-
	binarise(N,Pow2s),
	lm_prods(Up,Pow2s,M).

% Find the types of the positive and negative examples. Save these
%	reconsult from types.pl.

findtypes(PosNeg) :-
	findall(Ex1,(element(Ex,PosNeg),Ex=..[P,X,Y],
		type(X,Tx),type(Y,Ty),Ex1=[P,Tx,Ty]),Ts0),
	(sort(Ts0,[Ts]); writes(['\n','ERROR: NON-UNIQUE TYPES: ',Ts0,'\n']),
		!, fail),
	tell('modes.pl'), writes([modes(Ts),'.','\n']), told,
	reconsult(modes), !.
