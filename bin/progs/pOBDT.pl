if_then_else(Cond,Then,Else, Res):-
  bit(Cond),bit(Then),bit(Else),
  select_if_branch(Cond,Then,Else, Res).

select_if_branch(0,_,Else,Else).
select_if_branch(1,Then,_,Then).

bit(0).  bit(1).

all_ones_mask(NbOfBits,Mask):-Mask is (1<<(1<<NbOfBits))-1.   

var_to_bitstring_int(NbOfBits,K,Xk):-
  all_ones_mask(NbOfBits,Mask),
  var_to_bitstring_int(NbOfBits,Mask,K,Xk).

var_to_bitstring_int(NbOfBits,Mask,K,Xk):-
  NK is NbOfBits-(K+1),
  D is (1<<(1<<NK))+1,
  Xk is Mask//D.

shannon_split(NV,X, Hi,Lo):-
  all_ones_mask(NV,M),NV1 is NV-1,
  all_ones_mask(NV1,LM),
  HM is xor(M,LM),
  Lo is /\(LM,X),H is /\(HM,X),
  Hi is H>>(1<<NV1).

shannon_fuse(NV,Hi,Lo, X):-
  NV1 is NV-1,
  H is Hi<<(1<<NV1),
  X is \/(H,Lo).

% NV=number of variables, TT=a truth table, OBDT the result
shannon_tree(NV,TT, st(NV,OBDT)):-
  Max is (1<<(1<<NV)),
  TT<Max,
  shannon_unfold(NV,NV,TT, OBDT).

% recurses to depth NV, splitting TT into pairs
shannon_unfold(0,_,TT,c(TT)).
shannon_unfold(N,NV,TT,mux(X,H,L)):-N>0,
  N1 is N-1,
  X is NV-N,
  shannon_split(N,TT,Hi,Lo),
  shannon_unfold(N1,NV,Hi,H),
  shannon_unfold(N1,NV,Lo,L).

bitpair(X,Y,P):-
  to_rbits(X,Xs),
  to_rbits(Y,Ys),
  bitmix(Xs,Ys,Ps),
  from_rbits(Ps,P).

bitunpair(P,X,Y):-
  to_rbits(P,Ps),
  bitmix(Xs,Ys,Ps),
  from_rbits(Xs,X),
  from_rbits(Ys,Y).

bitmix([X|Xs],Ys,[X|Ms]):-!,bitmix(Ys,Xs,Ms).
bitmix([],[X|Xs],[0|Ms]):-bitmix([X|Xs],[],Ms).
bitmix([],[],[]).

unPairingTree(NV,TT, obdt(NV,UnPairingTree)):-
  Max is (1<<(1<<NV)), TT<Max,
  isplit(NV,TT, UnPairingTree).

isplit(0,TT,c(TT)).
isplit(NV,TT,ite(NV1,H,L)):-NV>0,
  NV1 is NV-1,
  bitunpair(TT,Hi,Lo),
  isplit(NV1,Hi,H),
  isplit(NV1,Lo,L).

uTreeReduce(Tree,obdt(NV,R)):-nonvar(Tree),
  Tree=obdt(NV,X),uTreeReduce1(X,R).

uTreeReduce1(c(TT),c(TT)).
uTreeReduce1(ite(_,A,B),R):-A==B,uTreeReduce1(A,R).
uTreeReduce1(ite(X,A,B),ite(X,RA,RB)):-A\==B,
  uTreeReduce1(A,RA),uTreeReduce1(B,RB).

reducedUnpairingTree(NV,TT, ReducedTree):-
  unPairingTree(NV,TT, Tree),
  uTreeReduce(Tree,ReducedTree).

pairingTree(obdt(_,X),TT):-pairingTree1(X,TT).

pairingTree1(c(TT),TT).
pairingTree1(ite(_,L,R),TT):-
  pairingTree1(L,X),
  pairingTree1(R,Y),
  bitpair(X,Y,TT).

evalOBDT(obdt(NV,B),TT):-
  all_ones_mask(NV,M),
  eval_with_mask(NV,M,B,TT).

eval_constant(0,_,0).
eval_constant(1,M,M).

eval_with_mask(_,M,c(X),R):-eval_constant(X,M,R).
eval_with_mask(NV,M,ite(X,T,E),R):-
  eval_with_mask(NV,M,T,A),
  eval_with_mask(NV,M,E,B),
  var_to_bitstring_int(NV,M,X,V),
  eval_ite(V,A,B,R).

eval_ite(X,T,E, R):-R is xor(/\(X,xor(T,E)),E).

bsum(0,0).
bsum(N,S):-N>0,N1 is N-1,bsum1(N1,S).

bsum1(0,2).
bsum1(N,S):-N>0,N1 is N-1,bsum1(N1,S1),S is S1+(1<<(1<<N)).

bsum(S):-nat(N),bsum(N,S).

nat(0).
nat(N):-nat(N1),N is N1+1.

to_bsum(N, X,N_M):-
  nat(X),bsum(X,S),S>N,!,
  K is X-1,
  bsum(K,M),
  N_M is N-M.

nat2unPairingTree(N,OBDT):-to_bsum(N, K,N_M),unPairingTree(K,N_M,OBDT).

nat2obdt(N,OBDT):-to_bsum(N, K,N_M),reducedUnpairingTree(K,N_M,OBDT).

unPairingTree2nat(obdt(NumberOfVars,OBDT),N) :-
  B=obdt(NumberOfVars,OBDT),
  pairingTree(B,Nth),
  K is NumberOfVars-1,
  bsum(K,S),N is S+Nth.

obdt2nat(obdt(NumberOfVars,OBDT),N) :-
  B=obdt(NumberOfVars,OBDT),
  evalOBDT(B,Nth),
  K is NumberOfVars-1,
  bsum(K,S),N is S+Nth.  

unPairingTree(OBDT):-nat(N),nat2unPairingTree(N,OBDT).

obdt(OBDT):-nat(N),nat2obdt(N,OBDT).

ranobdt(NumberOfVars,OBDT):-ranobdt(NumberOfVars,NumberOfVars,OBDT).

ranobdt(MinVars,MaxVars,OBDT):-
  MinVars1 is MinVars-1,
  bsum(MinVars1,Lower),
  bsum(MaxVars,Upper),
  Dif is Upper-Lower,
  Ran is random(Dif),
  N is Lower+Ran,
  nat2obdt(N,OBDT).

% converts an int to a list of bits, least significant first
to_rbits(0,[]).
to_rbits(N,[B|Bs]):-N>0,B is N mod 2, N1 is N//2,to_rbits(N1,Bs).

% converts a list of bits (least significant first) into an int
from_rbits(Rs,N):-nonvar(Rs),from_rbits(Rs,0,0,N).

from_rbits([],_,N,N).
from_rbits([X|Xs],E,N1,N3):-NewE is E+1,N2 is X<<E+N1,
  from_rbits(Xs,NewE,N2,N3).

