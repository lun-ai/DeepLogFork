%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	DeepLog Hypothesis Testing
%	Author: S.H. Muggleton
%	Date: 5th May, 2022
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% To run:
% $ swipl
% ?- [example,primitives1,hypothesis].

?- [interpret,compile,setarith,lmarith,utils,primitives1,hypothesis,modes,lib_dl].

% Test a single example. Assumes it is within the closure matrix

testprove(Ex,Subs,I,Cert) :-
	Ex=..[P,X,Y], maxcertdepth(MaxDepth),
	% writes(['** Testing example ',Ex,'\n']),
	% writes(['\n','MaxDepth=',MaxDepth,'\n']),
	find_min_depth(up,X,Y,1,MaxDepth,_),
	% writes(['\n','MinDepth=',MinDepth,'\n']),
	% interval(MinDepth,MaxDepth,Interval), element(Cert,Interval),
	interval(1,MaxDepth,Interval), element(Cert,Interval),
	dprove(P,X,Y,Subs,I,Cert), !. % , writes(['Succeded','\n']).
testprove(_,_,_,_) :- % writes(['Failed','\n']),
	!, fail.

% Test the hypothesis against the test set

% test(Problem,Test) :-
test(Problem) :-
	[Problem],
	% time(lmcompile([testpos,testneg],_,_,_)),
	Start is cputime,
	testpos(TestPos), length(TestPos,P),
	testneg(TestNeg), length(TestNeg,N),
	% hypmetasubs(Subs,I),
	% writes(['\n','Test Pos=',TestPos,'\n','\n']),
	% findall([ExPos,Cert],(element(ExPos,TestPos),testprove(ExPos,Subs,I,Cert)),TruePos),
	findall([ExPos],(element(ExPos,TestPos),once(call(ExPos))),TruePos),
	length(TruePos,TP), FP is P-TP,
	% findall([ExNeg],(element(ExNeg,TestNeg),once(testprove(ExNeg,Subs,I,Cert))),TrueNeg),
	findall([ExNeg],(element(ExNeg,TestNeg),(\+ call(ExNeg))),TrueNeg),
	length(TrueNeg,TN), FN is N-TN,
	% findall([ExPos],(element(ExPos,TestPos),(\+once(testprove(ExPos,Subs,I,Cert)))),FalseNeg),
	% writes(['FalseNeg list is ',FalseNeg,'\n','\n']),
	% findall(ExPos,(element(ExPos,TestPos),(\+ call(ExPos))),FalseNeg),
	% length(FalseNeg,FN),
	% findall([ExNeg,Cert],(element(ExNeg,TestNeg),once(testprove(ExNeg,Subs,I,Cert))),FalsePos),
	% writes(['FalsePos list is ',FalsePos,'\n','\n']),
	% length(FalsePos,FP),
	A is TP+FN, NotA is FP+TN, All is P+N,
	Accuracy is 100*((TP+TN)/All),
	writes(['Contingency table=    ________P________~P','\n']),
	writes(['                    A|        ',TP,'|        ',FP,'|         ',P,'\n']),
	writes(['                   ~A|        ',FN,'|        ',TN,'|         ',N,'\n']),
	writes(['                      ~~~~~~~~~~~~~~~~~~~','\n']),
	writes(['                              ',A,'         ',NotA,'         ',All,'\n']),
	writes(['Overall accuracy= ',Accuracy,'%','\n']),
	T is cputime-Start,
	writes(['Testing time taken: ',T,' seconds','\n']),
	!.


