cons(X,Y,XY):-X>=0,Y>=0,XY is (2^X)*(2*Y+1).

hd(XY,X):-XY>0,P is XY mod 2,hd1(P,XY,X).

  hd1(1,_,0).
  hd1(0,XY,X):-Z is XY // 2,hd(Z,H),X is H+1.

tl(XY,Y):-hd(XY,X),Y is XY // (2**(X+1)).

null(0).

list2nat([],0).
list2nat([X|Xs],N):-list2nat(Xs,N1),cons(X,N1,N).

nat2list(0,[]).
nat2list(N,[X|Xs]):-N>0,hd(N,X),tl(N,T),nat2list(T,Xs).

unrank(F,N,Rs):-call(F,N,Ns),maplist(unrank(F),Ns,Rs).

rank(G,Ts,Rs):-maplist(rank(G),Ts,Xs),call(G,Xs,Rs).

tsize(T,N) :- rank(tsize1,T,N).

  tsize1(Xs,N):-sumlist(Xs,S),N is S+1.

nat2hfseq(N,T):-unrank(nat2list,N,T).

hfseq2nat(T,N):-rank(list2nat,T,N).

s([],[[]]).
s([[K|Ks]|Xs],[[],K1|Xs]):-p([K|Ks],K1).
s([[]|Xs],[[K1|Ks]|Ys]):-s(Xs,[K|Ys]),s(K,[K1|Ks]). 

p([[]],[]).
p([[],K|Xs],[[K1|Ks]|Xs]):-s(K,[K1|Ks]).
p([[K|Ks]|Xs],[[]|Zs]):-p([K|Ks],K1),p([K1|Xs],Zs).

tree2nat([],0).
tree2nat([X|Xs],N):-p([X|Xs],Y),tree2nat(Y,M),N is M+1.

nat2tree(0,[]).
nat2tree(N,[X|Xs]):-N>0,M is N-1,nat2tree(M,Y),s(Y,[X|Xs]).

n([]).
n(S):-n(P),s(P,S).

slow_add([],X,X).
slow_add([X|Xs],Y,Z):-p([X|Xs],P),s(Y,Y1),slow_add(P,Y1,Z).

o_([[]|_]).
i_([[_|_]|_]).
e_([]).

o(X,[[]|X]).
i(X,Y):-s([[]|X],Y).

r([[]|Xs],Xs).
r([[X|Xs]|Ys],Rs):-p([[X|Xs]|Ys],[[]|Rs]).

s2n([],0).
s2n(X,R):-o_(X),r(X,S),s2n(S,N),R is 1+2*N.
s2n(X,R):-i_(X),r(X,S),s2n(S,N),R is 2+2*N.

n2s(0,[]).
n2s(N,R):-N>0,P is N mod 2,N1 is (N-1) // 2,
  n2s(N1,X),
  ( P=:=0->i(X,R)
  ; o(X,R)
  ).

a([],Y,Y).
a([X|Xs],[],[X|Xs]).
a(X,Y,Z):-o_(X),o_(Y),a1(X,Y,R),  i(R,Z).
a(X,Y,Z):-o_(X),i_(Y),a1(X,Y,R), a2(R,Z).
a(X,Y,Z):-i_(X),o_(Y),a1(X,Y,R), a2(R,Z).
a(X,Y,Z):-i_(X),i_(Y),a1(X,Y,R), s(R,S),i(S,Z).

  a1(X,Y,R):-r(X,RX),r(Y,RY),a(RX,RY,R).
  a2(R,Z):-s(R,S),o(S,Z).

m([],_,[]).
m(_,[],[]).
m(X,Y,Z):-p(X,X1),p(Y,Y1),m0(X1,Y1,Z1),s(Z1,Z).
  
m0([],Y,Y).
m0([[]|X],Y,[[]|Z]):- m0(X,Y,Z).
m0(X,Y, Z):-i_(X),r(X,X1),m0(X1,Y,Z1),a(Y,[[]|Z1],Y1),s(Y1,Z).

s_(e, (e->e)).
s_(((K->Ks)->Xs), (e->(K1->Xs))) :- p_((K->Ks), K1).
s_((e->Xs), ((K1->Ks)->Ys)) :- s_(Xs, (K->Ys)), s_(K, (K1->Ks)). 

p_((e->e), e).
p_((e->(K->Xs)), ((K1->Ks)->Xs)) :- s_(K, (K1->Ks)).
p_(((K->Ks)->Xs), (e->Zs)) :- p_((K->Ks), K1), p_((K1->Xs), Zs).

n_(e).
n_(S):-n_(P),s_(P,S).

t2n(e,0).
t2n((T->S),N):-p_((T->S),U),t2n(U,M),N is M+1.

pars_hfseq(Xs,T):-pars2term(0,1,T,Xs,[]).

pars2term(L,R,Xs) --> [L],pars2args(L,R,Xs).

pars2args(_,R,[]) --> [R].
pars2args(L,R,[X|Xs])-->pars2term(L,R,X),pars2args(L,R,Xs).   

kraft_sum(M,S):- M1 is M-1, numlist(0,M1,Ns),
  maplist(kraft_term,Ns,Ls),  
  sumlist(Ls,S).

kraft_term(N,X):-parsize(N,L), X is 1/2^L.

parsize(N,L):- nat2hfseq(N,HFSEQ), pars_hfseq(Xs,HFSEQ), length(Xs,L).

kraft_check(M):-kraft_sum(M,S),S=<1.

consUnPair(XY, X,Y):-Z is XY+1,hd(Z,X),tl(Z,Y).
consPair(X,Y, XY):-cons(X,Y,Z),XY is Z-1.

xml2nat(Xs,N):-xml2nat(N,Xs,[]).

xml2nat(0)-->[0,1].
xml2nat(N)-->
  [L],{X is (L-2) // 2},
  xml2nats(Es),
  [R],{R is L+1,list2nat(Es,E),consPair(X,E,N)}.

xml2nats([])-->[].
xml2nats([E|Es])-->xml2nat(E),xml2nats(Es).

nat2xml(0,[0,1]).
nat2xml(N,Xs):-N>0,
  consUnPair(N,LR,E),
  L is 2*LR+2,R is L+1,
  nat2list(E,Es),
  maplist(nat2xml,Es,Xss),append(Xss,Ys),
  append([L|Ys],[R],Xs).

apply_xml_dict(Xs,Dict,Ys):-maplist(apply_dict(Dict),Xs,Xss),append(Xss,Ys).

apply_dict(Dict,X,Ps):-I is X+1,arg(I,Dict,Ps).

sgram --> [].
sgram --> p,s.
sgram --> "(",sgram,")".

