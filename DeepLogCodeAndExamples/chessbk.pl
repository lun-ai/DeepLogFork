%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Chess Background Knowledge
%	Author: S.H. Muggleton
%	Date: 5th July, 2022
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples

%%%%%%%%%%%%%%%
% Chess moves
%	
%	 1 2 3 4 5 6 7 8      	 	  1 2 3 4 5 6 7 8
%      8               X 8            	8 X             X 8
%      7                 7      	7                 7
%      6                 6      	6                 6
%      5                 5      	5                 5
%      4                 4      	4                 4
%      3                 3      	3                 3
%      2                 2      	2                 2
%      1 A               1      	1               B 1
%	 1 2 3 4 5 6 7 8 	 	  1 2 3 4 5 6 7 8 

kn(X,Y) :- chsN(X,Z), kn_1(Z,Y).
kn_1(X,Y) :- chsE(X,Z), chsN(Z,Y).
kn_1(X,Y) :- chsN(X,Z), chsW(Z,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compass unit movements for agent
%	n
%	|
%  w ------ e
%	|
%	s

chsN(at(X,Y),at(X1,Y)) :- type(at(X,Y),place2d), chsrankorfile(X),
	succ(X,X1), chsrankorfile(X1), chsrankorfile(Y).
	% \+ block(at(X1,Y)).
chsE(at(X,Y),at(X,Y1)) :- type(at(X,Y),place2d), chsrankorfile(X),
	succ(Y,Y1), chsrankorfile(Y), chsrankorfile(Y1).
	% \+ block(at(X,Y1)).	
chsS(at(X,Y),at(X1,Y)) :- type(at(X,Y),place2d), chsrankorfile(X),
	pred(X,X1), chsrankorfile(X1), chsrankorfile(Y).
	% \+ block(at(X1,Y)).	
chsW(at(X,Y),at(X,Y1)) :- type(at(X,Y),place2d), chsrankorfile(X),
	pred(Y,Y1), chsrankorfile(Y), chsrankorfile(Y1).
	% \+ block(at(X,Y1)).	

chsInv(at(X,Y),at(Y,X)).

% pred(X,Y) :- succ(Y,X).

% succ(1,2). succ(2,3). succ(3,4). succ(4,5). succ(5,6).
% succ(6,7). succ(7,8).% succ(8,9). succ(9,10).
% succ(10,11).
% succ(11,12). succ(12,13). succ(13,14). succ(14,15).
% succ(15,16). succ(16,17). succ(17,18). succ(18,19). succ(19,20).

chsrankorfile(1). chsrankorfile(2). chsrankorfile(3). chsrankorfile(4). chsrankorfile(5).
chsrankorfile(6). chsrankorfile(7). chsrankorfile(8). % chsrankorfile(9). chsrankorfile(10).
% chsrankorfile(11). chsrankorfile(12). chsrankorfile(13). chsrankorfile(14). chsrankorfile(15).
% chsrankorfile(16). chsrankorfile(17). chsrankorfile(18). chsrankorfile(19). chsrankorfile(20).

% Kleene plus - one or more repetitions

% kleene(P,X,Y) :- call(P,X,Y).
% kleene(P,X,Y) :- call(P,X,Z), kleene(P,Z,Y).

% kleene(P,X,Y) :- call(P,X,Y).
% kleene(P,X,Y) :- call(P,X,Z), kleene(P,Z,Y).

% ns(X,Y) :- kleene(n,X,Y).
% es(X,Y) :- kleene(e,X,Y).
% ss(X,Y) :- kleene(s,X,Y).
% ws(X,Y) :- kleene(w,X,Y).

% one(P,S1,S2) :-
%         call(P,S1,S2).
% two(P,S1,S2) :-
%         one(P,S1,S3),
%         one(P,S3,S2).
% four(P,S1,S2) :-
%         two(P,S1,S3),
%         two(P,S3,S2).
% eight(P,S1,S2) :-
%         four(P,S1,S3),
%         four(P,S3,S2).

% n2(X,Y) :- two(n,X,Y). n4(X,Y) :- four(n,X,Y). n8(X,Y) :- eight(n,X,Y).
% e2(X,Y) :- two(e,X,Y). e4(X,Y) :- four(e,X,Y). e8(X,Y) :- eight(e,X,Y).
% s2(X,Y) :- two(s,X,Y). s4(X,Y) :- four(s,X,Y). s8(X,Y) :- eight(s,X,Y).
% w2(X,Y) :- two(w,X,Y). w4(X,Y) :- four(w,X,Y). w8(X,Y) :- eight(w,X,Y).

% block(_) :- !, fail.
