%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Library of Typed Background Predicates for DeepLog
%	Author: S.H. Muggleton
%	Date: 28th June, 2022
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [primitives,learned], learned(Ps), reconsult(Ps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge

%%%%%%%%%%%%%%%%%%%
% call1 - projects input onto higher dimension by adding
%	identity accumlulator variable of the appropriate type for return.

call1(X|Z,Ident|(X|Z)) :- modes([_,_,Ty]), ident(Ty,Ident).
call1(X,Ident|X) :- modes([_,Tx,Ty]), type(X,Tx), ident(Ty,Ident).

%%%%%%%%%%%%%%%%%%%
% return1 - projects onto lower dimension by returning
%	the accumulator value.

return1(Y|Ident,Y) :-  type(Ident,Type), ident(Type,Ident).
return1(Y|Ident|Rest,Y|Rest) :- type(Ident,Type), ident(Type,Ident).

%%%%%%%%%%%%%%%%%%%
% ident - provides ident elements for various data types. The analogy
%	is with the identity element e in a Mathematical Group.

ident(nat,0).		% Identity Element for Group <natural,+>
ident(nat,1).		% Identity Element for Group <natural,*>
ident(nl,nl).	% Identity Element for Group <nl,'.'>
ident(nll,nll).	% Identity Element for Group <nll,'.'>
ident(cl,cl).	% Identity Element for Group <cl,'.'>

%%%%%%%%%%%%%%%%%%%
% Auto-type recognition

type(N,nat) :- integer(N), N>=0.
% type(C,count) :- integer(C), C>0.
% type([],emptylist)
type(N|M,npair) :- integer(N), N>=0, integer(M), M>=0.
% type(X/(_),Tx)  :- type(X,Tx).
type(nl,nl).
type([H|T],nl) :- type(H,nat), type(T,nl).
type(X,char) :- char(X).
type(nll,nll).
type([H|T],nll) :- type(H,nl), type(T,nll).
type(cl,cl).
type([H|T],cl) :- type(H,char), type(T,cl), !.
type(X,atomic) :- atom(X), \+type(X,nat), \+type(X,char), \+type(X,cl).
type([H],atomiclist) :- type(H,atomic).
type([H|T],atomiclist) :- type(H,atomic), type(T,atomiclist).
type(at(X,Y),place2d) :- integer(X), integer(Y).
type(at(X,Y,Z),place3d) :- integer(X), integer(Y), integer(Z).

char(a). char(b). char(c). char(d). char(e). char(f). char(g). char(h).
char(i). char(j). char(k). char(l). char(m). char(n). char(o). char(p).
char(q). char(r). char(s). char(t). char(u). char(v). char(w). char(x).
char(y). char(z). 
char('A'). char('B'). char('C'). char('D'). char('E'). char('F'). char('G'). char('H'). 
char('I'). char('J'). char('K'). char('L'). char('M'). char('N'). char('O'). char('P'). 
char('Q'). char('R'). char('S'). char('T'). char('U'). char('V'). char('W'). char('X'). 
char('Y'). char('Z'). 

% char(a0). char(b0). char(c0). char(d0). char(e0). char(f0). char(g0). char(h0).
% char(i0). char(j0). char(k0). char(l0). char(m0). char(n0). char(o0). char(p0). char(q0). char(r0).

upcase(C|Stack,UpC|Stack) :- type(C,char), upc(C,UpC).

% upcase([X|In],[Y|In]) :- type([X|In],cl), upc(X,Y),

upc(a,'A'). upc(b,'B'). upc(c,'C'). upc(d,'D'). upc(e,'E'). upc(f,'F'). upc(g,'G'). 
upc(h,'H'). upc(i,'I'). upc(j,'J'). upc(k,'K'). upc(l,'L'). upc(m,'M'). upc(n,'N'). 
upc(o,'O'). upc(p,'P'). upc(q,'Q'). upc(r,'R'). upc(s,'S'). upc(t,'T'). upc(u,'U'). 
upc(v,'V'). upc(w,'W'). upc(x,'X'). upc(y,'Y'). upc(z,'Z'). 

mkloc(C|Stack,LoC|Stack) :- type(C,char), loc(C,LoC).

loc(X,Y) :- upc(Y,X).

pop(Z|[X|Y],X|Z|Y) :- type(X,char), type(Y,cl), type(Z,cl).
pop(Z|[X|Y],X|Z|Y) :- type(X,nat), type(Y,nl), type(Z,nl).

push(X|Z|Y,[X|Z]|Y) :- type(X,char), type(Y,cl), type(Z,cl).
push(X|Z|Y,[X|Z]|Y) :- type(X,nat), type(Y,nl), type(Z,nl).

popushaccum(Z|[X|Y],[X|Z]|Y) :- type(Y,nl), type(Z,nl).

popinc(X|[_|T],Y|T) :- type(X,nat), Y is X+1.

pop2(Sum|[H|T],H|Sum|T) :- type([H|T],nll).
% pop2(Sum/[H|T],H/Sum/T) :- type([H|T],nll).

prd2(S/nl/Nll) :- type(S,nat), type(Nll,nll).
prd2(S/[H|T]/Nll) :- type(S,nat), type([H|T]),
	type(Nll,nll).

eq(X,X).

% incaccum(X/L,Y/L) :- type(X,nat), type(L,nat), Y is X+1.
incaccum(X|L,Y|L) :- type(X,nat), type(L,nl), Y is X+1.

sumaccum(X|[Y|T],Z|T) :- type(X,nat), type([Y|T],nl), Z is X+Y.

sum12(X|Z|Rest,Y|Rest) :- type(X,nat), type(Z,nat), Y is X+Z.

prdaccum(X|[Y|T],Z|T) :- type(X,nat), type([Y|T],nl), Z is X*Y.
prdaccum(Prd|[X|T]|Rest,PrdX|(T|Rest)) :- type(Prd,nat),
	type([X|T],nl), PrdX is X*Prd.
% prdaccum(X/[Y|T],Z/T) :- type(X,nat), type([Y|T],nl), Z is X*Y.
% prdaccum(Prd/([X|T]/Rest),PrdX/(T/Rest)) :- type(Prd,nat),
%	type([X|T],nl), PrdX is X*Prd.

maxaccum(X|[Y|T],Z|T) :- type(X,nat), type([Y|T],nl), nmax(X,Y,Z).

minaccum(X|[Y|T],Z|T) :- type(X,nat), type([Y|T],nl), nmin(X,Y,Z).


nmax(X,Y,X) :- type(X,nat), type(Y,nat), X>=Y.
nmax(X,Y,Y) :- type(X,nat), type(Y,nat), Y>X.

nmin(X,Y,X) :- type(X,nat), type(Y,nat), X=<Y.
nmin(X,Y,Y) :- type(X,nat), type(Y,nat), Y<X.

succ(X,Y) :- type(X,nat), Y is X+1.

pred(X,Y) :- type(X,nat), Y is X-1.

timesdec(Ax|X,Ay|Y) :- times(Ax|X,Az|Z), dec(Az|Z,Ay|Y).

times(N|M,NM|M) :- type(N,nat), type(M,nat), NM is N*M.

incdec(Ax/X,Ay/Y) :- type(Ax/X,npair), inc(Ax/X,Az/Z), dec(Az/Z,Ay/Y).

dec(N|M,N|M1) :- type(N|M,npair), M1 is M-1.

inc(N|M,N|M1) :- type(N|M,npair), M1 is M+1.

times2(X,Y) :- modes([_,nat,nat]), type(X,nat), Y is X*2.

times10(X,Y) :- modes([_,nat,nat]), type(X,nat), Y is X*10.

square(X,Y) :- type(X,nat), Y is X*X.

a([a|T],T) :- type(T,cl). b([b|T],T) :- type(T,cl).
c([c|T],T) :- type(T,cl). d([d|T],T) :- type(T,cl).
e([e|T],T) :- type(T,cl). f([f|T],T) :- type(T,cl).
g([g|T],T) :- type(T,cl). h([h|T],T) :- type(T,cl).

i([i|T],T) :- type(T,cl). j([j|T],T) :- type(T,cl).
k([k|T],T) :- type(T,cl). l([l|T],T) :- type(T,cl).
m([m|T],T) :- type(T,cl). n([n|T],T) :- type(T,cl).
o([o|T],T) :- type(T,cl). p([p|T],T) :- type(T,cl).
q([q|T],T) :- type(T,cl). r([r|T],T) :- type(T,cl).

s([s|T],T) :- type(T,cl). t([t|T],T) :- type(T,cl).
u([u|T],T) :- type(T,cl). v([v|T],T) :- type(T,cl).
w([w|T],T) :- type(T,cl). x([x|T],T) :- type(T,cl).
y([y|T],T) :- type(T,cl). z([z|T],T) :- type(T,cl).

% a0([a0|T],T) :- type(T,cl). b0([b0|T],T) :- type(T,cl).
% c0([c0|T],T) :- type(T,cl). d0([d0|T],T) :- type(T,cl).
% e0([e0|T],T) :- type(T,cl). f0([f0|T],T) :- type(T,cl).
% g0([g0|T],T) :- type(T,cl). h0([h0|T],T) :- type(T,cl).
% i0([i0|T],T) :- type(T,cl). j0([j0|T],T) :- type(T,cl).
% k0([k0|T],T) :- type(T,cl). l0([l0|T],T) :- type(T,cl).
% m0([m0|T],T) :- type(T,cl). n0([n0|T],T) :- type(T,cl).
% o0([o0|T],T) :- type(T,cl). p0([p0|T],T) :- type(T,cl).
% q0([q0|T],T) :- type(T,cl). r0([r0|T],T) :- type(T,cl).

as(X,Y) :- a(X,Y).  as(X,Y) :- a(X,Z), as(Z,Y).
bs(X,Y) :- b(X,Y).  bs(X,Y) :- b(X,Z), bs(Z,Y).
cs(X,Y) :- c(X,Y).  cs(X,Y) :- c(X,Z), cs(Z,Y).
ds(X,Y) :- d(X,Y).  ds(X,Y) :- d(X,Z), ds(Z,Y).
es(X,Y) :- e(X,Y).  es(X,Y) :- e(X,Z), es(Z,Y).
% fs(X,Y) :- f(X,Y).  fs(X,Y) :- f(X,Z), fs(Z,Y).
gs(X,Y) :- g(X,Y).  gs(X,Y) :- g(X,Z), gs(Z,Y).
hs(X,Y) :- h(X,Y).  hs(X,Y) :- h(X,Z), hs(Z,Y).
iis(X,Y) :- i(X,Y).  iis(X,Y) :- i(X,Z), iis(Z,Y).
js(X,Y) :- j(X,Y).  js(X,Y) :- j(X,Z), js(Z,Y).
ks(X,Y) :- k(X,Y).  ks(X,Y) :- k(X,Z), ks(Z,Y).
ls(X,Y) :- l(X,Y).  ls(X,Y) :- l(X,Z), ls(Z,Y).
ms(X,Y) :- m(X,Y).  ms(X,Y) :- m(X,Z), ms(Z,Y).
ns(X,Y) :- n(X,Y).  ns(X,Y) :- n(X,Z), ns(Z,Y).
os(X,Y) :- o(X,Y).  os(X,Y) :- o(X,Z), os(Z,Y).
ps(X,Y) :- p(X,Y).  ps(X,Y) :- p(X,Z), ps(Z,Y).
qs(X,Y) :- q(X,Y).  qs(X,Y) :- q(X,Z), qs(Z,Y).
rs(X,Y) :- r(X,Y).  rs(X,Y) :- r(X,Z), rs(Z,Y).
ss(X,Y) :- s(X,Y).  ss(X,Y) :- s(X,Z), ss(Z,Y).
ts(X,Y) :- t(X,Y).  ts(X,Y) :- t(X,Z), ts(Z,Y).
us(X,Y) :- u(X,Y).  us(X,Y) :- u(X,Z), us(Z,Y).
vs(X,Y) :- v(X,Y).  vs(X,Y) :- v(X,Z), vs(Z,Y).
ws(X,Y) :- w(X,Y).  ws(X,Y) :- w(X,Z), ws(Z,Y).
xs(X,Y) :- x(X,Y).  xs(X,Y) :- x(X,Z), xs(Z,Y).
ys(X,Y) :- y(X,Y).  ys(X,Y) :- y(X,Z), ys(Z,Y).
zs(X,Y) :- z(X,Y).  zs(X,Y) :- z(X,Z), zs(Z,Y).

head([H|T],H) :- type([H|T],atomiclist).

tail([H|T],T) :- type([H|T],atomiclist).

unit([X],[X]) :- type([X],atomiclist).

empty([],[]).

apps(nll,nl).
apps([nl|T1],T2) :- apps(T1,T2).
apps([[H|T1]|T2],[H|T3]) :- apps([T1|T2],T3).

prdlist(X,Y) :- call1(X,Z), prdlist_1(Z,Y).
prdlist_1(X,Y) :- return1(X,Z), eq(Z,Y).
prdlist_1(X,Y) :- prdaccum(X,Z), prdlist_1(Z,Y).

sumlist(X,Y) :- call1(X,Z), sumlist_1(Z,Y).
sumlist_1(X,Y) :- return1(X,Z), eq(Z,Y).
sumlist_1(X,Y) :- sumaccum(X,Z), sumlist_1(Z,Y).

reversec(X,Y) :- call1(X,Z), reversec_1(Z,Y).
reversec_1(X,Y) :- eq(X,Z), return1(Z,Y).
reversec_1(X,Y) :- poppush(X,Z), reversec_1(Z,Y).
poppush(X,Y) :- pop(X,Z), push(Z,Y).

% Domain-specific background knowledge

:- [firebk,chessbk,familybk].
