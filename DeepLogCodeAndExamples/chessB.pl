%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Chess example
%	Author: S.H. Muggleton
%	Date: 12th October, 2021
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lib_dl].		% Common background knowledge

aux.

% Examples

pos([
	bishop(at(5,5),at(8,8)),
	bishop(at(4,4),at(5,5))
]).
neg([]).

testpos([
	bishop(at(2,2),at(7,7)),
	bishop(at(1,1),at(2,2)),
	bishop(at(2,3),at(6,7))
]).
testneg([
	bishop(at(2,3),at(6,5)),
	bishop(at(1,8),at(8,1)),
	bishop(at(6,8),at(2,3))
	]).

%%%%%%%%%%%%%%%
% Bishop moves
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

