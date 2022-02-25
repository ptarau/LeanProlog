compose(iso(F,G), iso(F1,G1), iso(fcompose(F1,F),fcompose(G,G1))).

itself(iso(id,id)).

invert(iso(F,G),iso(G,F)).

fcompose(G,F,X,Y):-call(F,X,Z),call(G,Z,Y).

id(X,X).

from(iso(F,_),X,Y):-call(F,X,Y).

to(iso(_,G),X,Y):-call(G,X,Y).

borrow(iso(F,G),Op,X,Y):-fcompose(F,fcompose(Op,G),X,Y).

lend(iso(G,F),Op,X,Y):-fcompose(F,fcompose(Op,G),X,Y).

with(IsoName1,IsoName2,Iso):-
  % gets the actual encoders by calling <Name>(Iso)
  call(IsoName1,Iso1), call(IsoName2,Iso2),
  invert(Iso2,Inv2),compose(Iso1,Inv2,Iso).
   
as(That,This,X,Y):-
  % routes through our hub
  with(That,This,Iso),
  % activates the isomorphism
  to(Iso,X,Y).

fun(Iso) :-itself(Iso).

cons(X,Y,XY):-X>=0,Y>=0,XY is (2**X)*(2*Y+1).
hd(XY,X):-XY>0,P is XY mod 2,hd1(P,XY,X).

hd1(1,_,0).
hd1(0,XY,X):-Z is XY // 2,hd(Z,H),X is H+1.

tl(XY,Y):-hd(XY,X),Y is XY // (2**(X+1)).

null(0).

fun2nat([],0).
fun2nat([X|Xs],N):-fun2nat(Xs,N1),cons(X,N1,N).

nat2fun(0,[]).
nat2fun(N,[X|Xs]):-N>0,hd(N,X),tl(N,T),nat2fun(T,Xs).

nat(iso(nat2fun,fun2nat)).

set(iso(set2fun,fun2set)).

set2fun(Xs,Ns):-sort(Xs,Is),set2fun(Is,-1,Ns).

set2fun([],_,[]).
set2fun([X|Xs],Y,[A|As]):-A is (X-Y)-1,set2fun(Xs,X,As).

fun2set(Ns,Xs):-fun2set(Ns,-1,Xs).

fun2set([],_,[]).
fun2set([X|Xs],Y,[A|As]):-A is (X+Y)+1,fun2set(Xs,A,As).

nat_set(iso(nat2set,set2nat)).

nat2set(N,Set):-nat2fun(N,F),fun2set(F,Set).
set2nat(Set,N):-set2fun(Set,F),fun2nat(F,N).

mset(iso(mset2fun,fun2mset)).

mset2fun(Xs,Ns):-msort(Xs,Is),mset2fun(Is,0,Ns).

mset2fun([],_,[]).
mset2fun([X|Xs],Y,[A|As]):-A is (X-Y),mset2fun(Xs,X,As).

fun2mset(Ns,Xs):-fun2mset(Ns,0,Xs).

fun2mset([],_,[]).
fun2mset([X|Xs],Y,[A|As]):-A is (X+Y),fun2mset(Xs,A,As).

unrank(F,N,R):-call(F,N,Y),unranks(F,Y,R).
unranks(F,Ns,Rs):-maplist(unrank(F),Ns,Rs).

rank(G,Ts,Rs):-ranks(G,Ts,Xs),call(G,Xs,Rs).
ranks(G,Ts,Rs):-maplist(rank(G),Ts,Rs).

tsize1(Xs,N):-sumlist(Xs,S),N is S+1.

tsize(T,N) :- rank(tsize1,T,N).

hylo(IsoName,iso(rank(G),unrank(F))):-
  call(IsoName,iso(F,G)).

hylos(IsoName,iso(ranks(G),unranks(F))):-
  call(IsoName,iso(F,G)).

hfs(Iso):-
  hylo(nat_set,Hylo),
  nat(Nat),
  compose(Hylo,Nat,Iso).

hfs_succ(H,R):-with(nat,hfs,Iso),borrow(Iso,succ,H,R).

ackermann(H,N):-as(nat,hfs,H,N).
inverse_ackermann(N,H):-as(hfs,nat,N,H).

hff(Iso) :- 
  hylo(nat,Hylo),nat(Nat),
  compose(Hylo,Nat,Iso).  

pars2hff(Xs,T):-pars2term(0,1,T,Xs,[]).

pars2term(L,R,Xs) --> [L],pars2args(L,R,Xs).

pars2args(_,R,[]) --> [R].
pars2args(L,R,[X|Xs])-->
   pars2term(L,R,X),
   pars2args(L,R,Xs).   

pars(Iso):-hff(HFF),
  compose(iso(pars2hff,hff2pars),HFF,Iso).

hff2pars(HFF,Ps):-pars2hff(Ps,HFF).

% factoradics of N, right to left
fr(0,[0]).
fr(N,R):-N>0,fr1(1,N,R).
   
fr1(_,0,[]).
fr1(J,K,[KMJ|Rs]):-K>0,
  KMJ is K mod J,J1 is J+1,KDJ is K // J,
  fr1(J1,KDJ,Rs).

fl(N,Ds):-fr(N,Rs),reverse(Rs,Ds).

lf(Ls,S):-length(Ls,K),K1 is K-1,lf(K1,_,S,Ls,[]).

% from list of digits of factoradics, back to decimals
lf(0,1,0)-->[0].
lf(K,N,S)-->[D],
  {K>0,K1 is K-1},lf(K1,N1,S1),
  {N is K*N1,S is S1+D*N}.

rf(Ls,S):-reverse(Ls,Rs),lf(Rs,S).

perm2nth(Ps,Size,N):-
  length(Ps,Size),Last is Size-1,
  ints_from(0,Last,Is),
  perm_lehmer(Is,Ps,Ls),
  lf(Ls,N).

% associates Lehmer code to a permutation 
perm_lehmer([],[],[]).
perm_lehmer(Xs,[X|Zs],[K|Ks]):-
  select_and_remember(X,Xs,Ys,0,K),
  perm_lehmer(Ys,Zs,Ks).

% remembers selections - for Lehmer code
select_and_remember(X,[X|Xs],Xs,K,K).
select_and_remember(X,[Y|Xs],[Y|Ys],K1,K3):-K2 is K1+1,
  select_and_remember(X,Xs,Ys,K2,K3).

nth2perm(Size,N, Ps):-
  fl(N,Ls),length(Ls,L),
  K is Size-L,Last is Size-1,
  ints_from(0,Last,Is),ndup(K,0,Zs),
  append(Zs,Ls,LehmerCode),
  perm_lehmer(Is,Ps,LehmerCode).

% fast computation of the sum of all factorials up to N!
sf(0,0).
sf(N,R1):-N>0,N1 is N-1,
  ndup(N1,1,Ds),
  rf([0|Ds],R),R1 is R+1.

sf(S):-nat_stream(N),sf(N,S).

nat_stream(0).
nat_stream(N):-nat_stream(N1),N is N1+1.

to_sf(N, K,N_M):-
  nat_stream(X),sf(X,S),S>N,
  !,
  K is X-1,
  sf(K,M),N_M is N-M.

nat2perm(0,[]).
nat2perm(N,Ps):-to_sf(N, K,N_M),nth2perm(K,N_M,Ps).

perm2nat([],0).
perm2nat(Ps,N) :-
  perm2nth(Ps, Size,Nth),
  sf(Size,S),
  N is S+Nth.

perm(Iso):-nat(Nat),compose(iso(perm2nat,nat2perm),Nat,Iso).

nat_perm(iso(nat2perm,perm2nat)).

hfp(Iso):-
  hylo(nat_perm,Hylo),
  nat(Nat),
  compose(Hylo,Nat,Iso).

cantor_pair(K1,K2,P):-P is (((K1+K2)*(K1+K2+1))//2)+K2.

cantor_unpair(Z,K1,K2):-
  E is 8*Z+1,
  intSqrt(E,R),
  I is (R-1)//2,
  K1 is ((I*(3+I))//2)-Z,
  K2 is Z-((I*(I+1))//2).

bitpair(X,Y,P):-
  to_base(2,X,Xs),to_base(2,Y,Ys),
  bitmix(Xs,Ys,Ps),
  from_base(2,Ps,P).

bitunpair(P,X,Y):-
  to_base(2,P,Ps),
  bitmix(Xs,Ys,Ps),
  from_base(2,Xs,X),from_base(2,Ys,Y).

bitmix([X|Xs],Ys,[X|Ms]):-!,bitmix(Ys,Xs,Ms).
bitmix([],[X|Xs],[0|Ms]):-bitmix([X|Xs],[],Ms).
bitmix([],[],[]).

bitpair(X-Y,Z):-bitpair(X,Y,Z).

bitunpair(Z,X-Y):-bitunpair(Z,X,Y).

bnat2(Iso):-nat(Nat),
  compose(iso(bitpair,bitunpair),Nat,Iso).

consUnPair(XY,X-Y):-Z is XY+1,hd(Z,X),tl(Z,Y).
consPair(X-Y,XY):-cons(X,Y,Z),XY is Z-1.

nat2(Iso):-nat(Nat),
  compose(iso(consPair,consUnPair),Nat,Iso).

cdigraph2set(Ps,Ns) :- maplist(consPair,Ps,Ns).
set2cdigraph(Ns,Ps) :- maplist(consUnPair,Ns,Ps).

cdigraph(Iso):-set(Set),
   compose(iso(cdigraph2set,set2cdigraph),Set,Iso).

digraph2set(Ps,Ns) :- maplist(bitpair,Ps,Ns).
set2digraph(Ns,Ps) :- maplist(bitunpair,Ns,Ps).

digraph(Iso):-set(Set),
   compose(iso(digraph2set,set2digraph),Set,Iso).

dig2dag(Dig,Dag):-maplist(shiftEdge,Dig,Dag).
shiftEdge(X-Y,X-NewY):-NewY is Y+X+1.

dag2dig(Dag,Dig):-maplist(unShiftEdge,Dag,Dig).
unShiftEdge(X-Y,X-NewY):-Y>X,NewY is Y-X-1.

dag(DagIso):-digraph(DigIso),
   compose(iso(dag2dig,dig2dag),DigIso,DagIso).

set2hypergraph(S,G) :- maplist(nat2nonempty,S,G).
hypergraph2set(G,S) :- maplist(nonempty2nat,G,S).

nat2nonempty(N,S):-N1 is N+1,nat2set(N1,S).
nonempty2nat(S,N):-set2nat(S,N1),N is N1-1.

hypergraph(Iso):-set(Set),
  compose(iso(hypergraph2set,set2hypergraph),Set,Iso).

app(0,Ys,Ys).
app(XXs,Ys,XZs):-XXs>0,hd(XXs,X), tl(XXs,Xs),
  app(Xs,Ys,Zs), cons(X,Zs,XZs).

nfoldl(_,Z,0,Z).
nfoldl(F,Z,Xs,R):-Xs>0,
  hd(Xs,H),tl(Xs,Ts),
  call(F,Z,H,H1),
  nfoldl(F,H1,Ts,R).

nfoldr(_,Z,0,Z).
nfoldr(F,Z,Xs,R):-
  hd(Xs,H),tl(Xs,Ts),
  nfoldr(F,Z,Ts,T),
  call(F,H,T,R).

nrev(Xs,Ys):-nfoldl(revcons,0,Xs,Ys).

  revcons(Xs,X,Ys):-cons(X,Xs,Ys).

nmaplist(F,Xs,Ys):-nfoldr(fcons(F),0,Xs,Ys).

  fcons(F,X,Xs,Ys):-call(F,X,Y),cons(Y,Xs,Ys).

nappend(Xss,Ys):-nfoldr(app,0,Xss,Ys). 

getAssoc(_,0,0).
getAssoc(K,Ps,V):-K>0,hd(Ps,XY),getAssoc1(XY,K,Ps,V).

getAssoc1(0,_,0).
getAssoc1(XY,K,Ps,V):-tl(Ps,Qs),hd(XY,X),tl(XY,Y),
  getAssoc2(K,X,Y,Qs,V).

getAssoc2(K,K,Y,_,Y).
getAssoc2(K,X,_,Qs,V):-K=\=X,getAssoc(K,Qs,V).

addAssoc(K,V,Ps,Qs):-cons(K,V,KV),cons(KV,Ps,Qs).

nth(Thing,N,X) :- as(Thing,nat,N,X).

stream_of(Thing,X) :- nat_stream(N),nth(Thing,N,X).

random_gen(Thing,Max,Len,X):-random_fun(Max,Len,Ns),
  as(Thing,fun,Ns,X).
  
random_fun(Max,Len,Ns):-length(Ns,Len),
  maplist(random_nat(Max),Ns).

random_nat(Max,N):-random(X),N is integer(Max*X).

term2bitpars(T,Ps,As):-term2bitpars(T,Ps,[],As,[]).

term2bitpars(T,Ps,Ps)-->{var(T)},[T].
term2bitpars(T,Ps,Ps)-->{atomic(T)},[T].
term2bitpars(T,[0|Ps],NewPs)-->{compound(T),T=..Xs},
  args2bitpars(Xs,Ps,NewPs).
  
args2bitpars([],[1|Ps],Ps)-->[].
args2bitpars([X|Xs],[0|Ps],NewPs)-->
  term2bitpars(X,Ps,[1|XPs]),
  args2bitpars(Xs,XPs,NewPs).  

bitpars2term(Ps,As,T):-bitpars2term(T,Ps,[],As,[]).

bitpars2term(T,Ps,Ps)-->[T].
bitpars2term(T,[0|Ps],NewPs)-->
  bitpars2args(Xs,Ps,NewPs),{T=..Xs}.
  
bitpars2args([],[1|Ps],Ps)-->[].
bitpars2args([X|Xs],[0|Ps],NewPs)-->
  bitpars2term(X,Ps,[1|XPs]),
  bitpars2args(Xs,XPs,NewPs).  

term2code(T,N,As):-term2bitpars(T,Ps,As),
  from_base(2,Ps,N).
  
code2term(N,As,T):-to_base(2,N,Ps),
  bitpars2term(Ps,As,T).

% conversion to list of digits in given base
to_base(Base,N,Bs):-to_base(N,Base,0,Bs).

to_base(N,R,_K,Bs):-N<R,Bs=[N].
to_base(N,R,K,[B|Bs]):-N>=R,
  B is N mod R, N1 is N//R,K1 is K+1,
  to_base(N1,R,K1,Bs).

% conversion from list of digits in given base
from_base(_Base,[],0).
from_base(Base,[X|Xs],N):-
  from_base(Base,Xs,R),N is X+R*Base.  

% generates integers From..To
ints_from(From,To,Is):-findall(I,between(From,To,I),Is).

% replicates X, N times
ndup(0, _,[]).
ndup(N,X,[X|Xs]):-N>0,N1 is N-1,ndup(N1,X,Xs).

% computes integer square root using Newton's method
intSqrt(0,0).
intSqrt(N,R):-N>0,
  iterate(N,N,K),K2 is K*K,
  (K2>N->R is K-1;R=K).

% iterates until close enough   
iterate(N,X,NewR):-
  R is (X+(N//X))//2,
  A is abs(R-X),
  (A<2->NewR=R;iterate(N,R,NewR)).

show_hfs(S):-gshow(S,"{,}"),nl.
show_hfp(S):-gshow(S,"( )"),nl.

gshow(0,[L,_C,R]):-put(L),put(R).
gshow(N,_):-integer(N),N>0,!,write(N).
gshow(Hs,[L,C,R]):-put(L),gshow_all(Hs,[L,C,R]),put(R).

gshow_all([],_).
gshow_all([H],LCR):-gshow(H,LCR).
gshow_all([H,G|Hs],[L,C,R]):-
  gshow(H,[L,C,R]),
  ([C]\=="~"->put(C);true),
  gshow_all([G|Hs],[L,C,R]).

bm:-time(bmgoal(L)),write(len=L),nl,fail;true.

bmgoal(L):-N is 10^1000,as(perm,nat,N,Ns),length(Ns,L).
