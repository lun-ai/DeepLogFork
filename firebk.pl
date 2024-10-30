%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Fire16 example - Background knowledge
%		Fire escape plan for a 16 story building
%	Author: S.H. Muggleton
%	Date: 17th May, 2022
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
% Floorplan
%	
%	 1 2 3 4 5 6 7 8 9 10
%     10                      10
%      9   * * * * * * * *    9
%      8   *           A *    8
%      7   *   * * * * * *    7
%      6   *   *       B *    6
%      5   *   *   * * * *    5
%      4   *   *   *   C *    4
%      3   *   *   *     *    3
%      2   *   *   *   * *    2
%      1 S *                  1
%	 1 2 3 4 5 6 7 8 9 10
%		FLOOR2
%
%	 1 2 3 4 5 6 7 8 9 10
%     10                   X 10
%      9   * * * * * * * *   9
%      8   *             *   8
%      7   *   * * * * * *   7
%      6   *   *         *   6
%      5   *   *   * * * *   5
%      4   *   *   *     *   4
%      3   *   *   *     *   3
%      2   *   *   *   * *   2
%      1 S *                 1
%	 1 2 3 4 5 6 7 8 9 10
%		FLOOR1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generative type definition for X and Y arguments of instances

num16(1). num16(2). num16(3). num16(4). num16(5).
num16(6). num16(7). num16(8). num16(9). num16(10).
num16(11). num16(12). num16(13). num16(14). num16(15).
num16(16).

typeX(at(X,Y,Z)) :- num16(X),num16(Y),num16(Z),X=<10,Y=<10,Z=<16.
typeY(at(10,10,1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge

% Longitude and latitude bounds

latbound(X) :- rankorfile(X), 1=<X, X=<10.
lngbound(Y) :- rankorfile(Y), 1=<Y, Y=<10.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compass unit movements for agent
%	n
%	|
%  w ------ e
%	|
%	s

fn(S1,S2) :- type(S1,place3d),
	\+ block(S1),
	S1 = at(X,Y,Z), S2 = at(X1,Y,Z), suc20(X,X1), latbound(X1), % Move North
	lngbound(Y), floor(Z), \+ block(S2).
fe(S1,S2) :- type(S1,place3d),
	\+ block(S1),
	S1 = at(X,Y,Z), S2 = at(X,Y1,Z), latbound(X), % Move East
	suc20(Y,Y1), lngbound(Y1), floor(Z), \+ block(S2).
fs(S1,S2) :- type(S1,place3d),
	\+ block(S1),
	S1 = at(X,Y,Z), S2 = at(X1,Y,Z), pred20(X,X1), latbound(X1), % Move South
	latbound(Y), floor(Z), \+ block(S2).
fw(S1,S2) :- type(S1,place3d),
	\+ block(S1),
	S1 = at(X,Y,Z), S2 = at(X,Y1,Z), latbound(X), % Move West
	pred20(Y,Y1), lngbound(Y1), floor(Z), \+ block(S2).
fd(S1,S2) :- type(S1,place3d),
	\+ block(S1),
	stair(S1),
	S1 = at(X,Y,Z), S2 = at(X,Y,Z1),		% Move Down a floor
	pred20(Z,Z1), floor(Z1), latbound(X), lngbound(Y).
fu(S1,S2) :- type(S1,place3d), 
	stair(S1),
	S1 = at(X,Y,Z), S2 = at(X,Y,Z1),		% Move Up a floor
	suc20(Z,Z1), floor(Z1), latbound(X), lngbound(Y).

pred20(X,Y) :- suc20(Y,X).

suc20(1,2). suc20(2,3). suc20(3,4). suc20(4,5). suc20(5,6).
suc20(6,7). suc20(7,8). suc20(8,9). suc20(9,10).
suc20(10,11).
suc20(11,12). suc20(12,13). suc20(13,14). suc20(14,15).
suc20(15,16). % suc20(16,17). suc20(17,18). suc20(18,19). suc20(19,20).

rankorfile(1). rankorfile(2). rankorfile(3). rankorfile(4). rankorfile(5).
rankorfile(6). rankorfile(7). rankorfile(8). rankorfile(9). rankorfile(10).
rankorfile(11). rankorfile(12). rankorfile(13). rankorfile(14). rankorfile(15).
rankorfile(16). rankorfile(17). rankorfile(18). rankorfile(19). rankorfile(20).

floor(N) :- rankorfile(N), 1=<N, N=<16.

% Kleene plus - one or more repetitions

% kleene(P,X,Y) :- call(P,X,Y).
% kleene(P,X,Y) :- call(P,X,Z), kleene(P,Z,Y).

kleene(P,X,Y) :- call(P,X,Y).
kleene(P,X,Y) :- call(P,X,Z), kleene(P,Z,Y).

fns(X,Y) :- type(X,place3d), kleene(fn,X,Y).
fes(X,Y) :- type(X,place3d), kleene(fe,X,Y).
fss(X,Y) :- type(X,place3d), kleene(fs,X,Y).
fws(X,Y) :- type(X,place3d), kleene(fw,X,Y).

one(P,S1,S2) :-
        call(P,S1,S2).
two(P,S1,S2) :-
        one(P,S1,S3),
        one(P,S3,S2).
four(P,S1,S2) :-
        two(P,S1,S3),
        two(P,S3,S2).
eight(P,S1,S2) :-
        four(P,S1,S3),
        four(P,S3,S2).

n2(X,Y) :- two(n,X,Y). n4(X,Y) :- four(n,X,Y). n8(X,Y) :- eight(n,X,Y).
e2(X,Y) :- two(e,X,Y). e4(X,Y) :- four(e,X,Y). e8(X,Y) :- eight(e,X,Y).
s2(X,Y) :- two(s,X,Y). s4(X,Y) :- four(s,X,Y). s8(X,Y) :- eight(s,X,Y).
w2(X,Y) :- two(w,X,Y). w4(X,Y) :- four(w,X,Y). w8(X,Y) :- eight(w,X,Y).


% open(S1,S2) :- S1 is_set [at(1,4)],
%	S2 is_set [at(1,3)].	% opens door1 to westward movement
% open(S1,S2) :- S1 is_set [at(1,2)],
%	S2 is_set [at(1,3)].	% opens door1 to eastward movement


blocked(S) :- block(B), element(B,S).

% %	 1 2 3 4 5 6 7 8 9 10
% %     10                      10
% %      9   * * * * * * * *    9
% %      8   *           A *    8
% %      7   *   * * * * * *    7
% %      6   *   *       B *    6
% %      5   *   *   * * * *    5
% %      4   *   *   *   C *    4
% %      3   *   *   *     *    3
% %      2   *   *   *   * *    2
% %      1 S *                  1
% %	 1 2 3 4 5 6 7 8 9 10
% %		FLOOR3
% 
% block(at(X,2,3)) :- rankorfile(X), 1=<X, X=<8.	% Long North/South Wall
% block(at(9,Y,3)) :- rankorfile(Y), 2=<Y, Y=<9.	% Long East/West Wall
% block(at(X,9,3)) :- rankorfile(X), 2=<X, X=<8.	% Rightmost North/South Wall
% block(at(2,8,3)).
% block(at(5,Y,3)) :- rankorfile(Y), 6=<Y, Y=<8.	% Short East/West Wall
% block(at(X,6,3)) :- rankorfile(X), 2=<X, X=<4.	% Short North/South Wall
% block(at(7,Y,3)) :- rankorfile(Y), 4=<Y, Y=<8.	% Medium East/West Wall
% block(at(X,4,3)) :- rankorfile(X), 2=<X, X=<6.	% Rightmost North/South Wall
% 
% %	 1 2 3 4 5 6 7 8 9 10
% %     10                      10
% %      9   * * * * * * * *    9
% %      8   *             *    8
% %      7   *   * * * * * *    7
% %      6   *   *         *    6
% %      5   *   *   * * * *    5
% %      4   *   *   *     *    4
% %      3   *   *   *     *    3
% %      2   *   *   *   * *    2
% %      1 S *                  1
% %	 1 2 3 4 5 6 7 8 9 10
% %		FLOOR2
% 
% block(at(X,2,2)) :- rankorfile(X), 1=<X, X=<8.	% Long North/South Wall
% block(at(9,Y,2)) :- rankorfile(Y), 2=<Y, Y=<9.	% Long East/West Wall
% block(at(X,9,2)) :- rankorfile(X), 2=<X, X=<8.	% Rightmost North/South Wall
% block(at(2,8,2)).
% block(at(5,Y,2)) :- rankorfile(Y), 6=<Y, Y=<8.	% Short East/West Wall
% block(at(X,6,2)) :- rankorfile(X), 2=<X, X=<4.	% Short North/South Wall
% block(at(7,Y,2)) :- rankorfile(Y), 4=<Y, Y=<8.	% Medium East/West Wall
% block(at(X,4,2)) :- rankorfile(X), 2=<X, X=<6.	% Rightmost North/South Wall

%	 1 2 3 4 5 6 7 8 9 10
%     10                   X 10
%      9   * * * * * * * *   9
%      8   *             *   8
%      7   *   * * * * * *   7
%      6   *   *         *   6
%      5   *   *   * * * *   5
%      4   *   *   *     *   4
%      3   *   *   *     *   3
%      2   *   *   *   * *   2
%      1 S *                 1
%	 1 2 3 4 5 6 7 8 9 10
%		FLOOR(N)

block(at(X,2,N)) :- floor(N), rankorfile(X),
	1=<X, X=<9.	% Long North/South Wall
block(at(9,Y,N)) :- floor(N), rankorfile(Y),
	2=<Y, Y=<9.	% Long East/West Wall
block(at(X,9,N)) :- floor(N), rankorfile(X),
	2=<X, X=<9.	% Rightmost North/South Wall
block(at(2,8,N)) :- floor(N).
block(at(5,Y,N)) :- floor(N), rankorfile(Y),
	6=<Y, Y=<9.	% Short East/West Wall
block(at(X,6,N)) :- floor(N), rankorfile(X),
	2=<X, X=<5.	% Short North/South Wall
block(at(7,Y,N)) :- floor(N), rankorfile(Y),
	4=<Y, Y=<9.	% Medium East/West Wall
block(at(X,4,N)) :- floor(N), rankorfile(X),
	2=<X, X=<7.	% Second leftmost North/South Wall

stair(at(1,1,Z)) :- rankorfile(Z), floor(Z).
