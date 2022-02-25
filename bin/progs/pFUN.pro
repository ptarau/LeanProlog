unrank_(Ulimit,_,N,R):-N>=0,N<Ulimit,!,R=N.
unrank_(Ulimit,F,N,R):-N>=Ulimit,
  N0 is N-Ulimit,
  call(F,N0,Ns),
  maplist(unrank_(Ulimit,F),Ns,R).

default_ulimit(0)-->[].

unrank(F)-->default_ulimit(Ulimit),unrank_(Ulimit,F).

rank_(Ulimit,_,N,R):-integer(N),N>=0,N<Ulimit,!,R=N.
rank_(Ulimit,G,Ts,R):-maplist(rank_(Ulimit,G),Ts,T),call(G,T,R0),R is R0+Ulimit.

rank(G)-->default_ulimit(Ulimit),rank_(Ulimit,G).  

set2nat(Xs,N):-set2nat(Xs,0,N).

set2nat([],R,R).
set2nat([X|Xs],R1,Rn):-R2 is R1+(1<<X),set2nat(Xs,R2,Rn).

hfs2nat-->default_ulimit(Ulimit),hfs2nat_(Ulimit).

hfs2nat_(Ulimit)-->rank_(Ulimit,set2nat).

nat2set(N,Xs):-nat2elements(N,Xs,0).

nat2elements(0,[],_K).
nat2elements(N,NewEs,K1):-N>0,
  B is /\(N,1),N1 is N>>1,K2 is K1+1,add_el(B,K1,Es,NewEs),
  nat2elements(N1,Es,K2).

add_el(0,_,Es,Es).
add_el(1,K,Es,[K|Es]).

nat2hfs_(Ulimit)-->unrank_(Ulimit,nat2set).

nat2hfs-->default_ulimit(Ulimit),nat2hfs_(Ulimit).

pepis_pair(X,Y,Z):-pepis_J(X,Y,Z).

pepis_unpair(Z,X,Y):-pepis_K(Z,X),pepis_L(Z,Y).
 
pepis_J(X,Y, Z):-Z is ((1<<X)*((Y<<1)+1))-1.
pepis_K(Z, X):-Z1 is Z+1,two_s(Z1,X).
pepis_L(Z, Y):-Z1 is Z+1,no_two_s(Z1,N),Y is (N-1)>>1. 

two_s(N,R):-even(N),!,H is N>>1,two_s(H,T),R is T+1.
two_s(_,0).

no_two_s(N,R):-two_s(N,T),R is N // (1<<T).

even(X):- 0 =:= /\(1,X).

to_tuple(K,N, Ns):-
  Base is 1<<K,to_base(Base,N,Ds),maplist(to_maxbits(K),Ds,Bss),
  mtranspose(Bss,Xss),
  maplist(from_rbits,Xss,Ns). 

from_tuple(Nss,R):-
  max_bitcount(Nss,L),length(Nss,K),maplist(to_maxbits(L),Nss,Mss),
  mtranspose(Mss,Tss),
  maplist(from_rbits,Tss,Ts),Base is 1<<K,from_base(Base,Ts,R).

to_pair(N,A,B):-to_tuple(2,N,[A,B]).

from_pair(X,Y,Z):-from_tuple([X,Y],Z).

ftuple2nat([],0).
ftuple2nat(Ns, N):-Ns=[_|_],
  length(Ns,K),K1 is K-1, 
  from_tuple(Ns,T),pepis_pair(K1,T, N).

nat2ftuple(0,[]).  
nat2ftuple(N,Ns):-N>0,
  pepis_unpair(N,K,F),K1 is K+1,
  to_tuple(K1,F,Ns).

nat(0).
nat(N):-nat(N1),N is N1+1.

iterative_fun_generator(F):-nat(N),nat2ftuple(N,F).

fun2set([],[]).
fun2set([A|As],Xs):-findall(X,prefix_sum(A,As,X),Xs).

prefix_sum(A,As,R):-append(Ps,_,As),length(Ps,L),
  sumlist(Ps,S),R is A+S+L.

set2fun([],[]).
set2fun([X|Xs],[X|Fs]):-
  sort([X|Xs],[_|Ys]),
  set2fun(Ys,X,Fs).

set2fun([],_,[]).
set2fun([X|Xs],Y,[A|As]):-A is (X-Y)-1,set2fun(Xs,X,As).

nat2fun --> nat2set,set2fun.

fun2nat --> fun2set,set2nat.

bits2rle([],[]):-!.
bits2rle([_],[0]):-!.
bits2rle([X,Y|Xs],Rs):-X==Y,!,bits2rle([Y|Xs],[C|Cs]),C1 is C+1,Rs=[C1|Cs].
bits2rle([_|Xs],[0|Rs]):-bits2rle(Xs,Rs).

rle2bits([],[]).
rle2bits([N|Ns],NewBs):-rle2bits(Ns,Xs),
  ( []==Xs->B is 1
  ; Xs=[X1|_],B is 1-X1
  ),
  N1 is N+1,ndup(N1,B,Bs),append(Bs,Xs,NewBs).

nat2rle --> to_rbits0,bits2rle.
rle2nat --> rle2bits,from_rbits .

to_rbits0(0,[]).
to_rbits0(N,R):-N>0,to_rbits(N,R).

nat2hff --> default_ulimit(D),nat2hff_(D).   
nat2hff1 --> default_ulimit(D),nat2hff1_(D).
nat2hff2  --> default_ulimit(D),nat2hff2_(D).

nat2hff_(Ulimit) --> unrank_(Ulimit,nat2fun).
nat2hff1_(Ulimit) --> unrank_(Ulimit,nat2ftuple).
nat2hff2_(Ulimit) --> unrank_(Ulimit,nat2rle).

hff2nat --> rank(fun2nat).
hff2nat1 --> rank(ftuple2nat).
hff2nat2 --> rank(rle2nat).

% factoradics of N, right to left
fr(0,[0]).
fr(N,R):-N>0,fr1(1,N,R).
   
fr1(_,0,[]).
fr1(J,K,[KMJ|Rs]):-K>0,KMJ is K mod J,J1 is J+1,KDJ is K // J,
  fr1(J1,KDJ,Rs).

fl(N,Ds):-fr(N,Rs),reverse(Rs,Ds).

lf(Ls,S):-length(Ls,K),K1 is K-1,lf(K1,_,S,Ls,[]).

% from list of digits of factoradics, back to decimals
lf(0,1,0)-->[0].
lf(K,N,S)-->[D],{K>0,K1 is K-1},lf(K1,N1,S1),{N is K*N1,S is S1+D*N}.

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
  K is Size-L,Last is Size-1,ints_from(0,Last,Is),
  zeros(K,Zs),append(Zs,Ls,LehmerCode),
  perm_lehmer(Is,Ps,LehmerCode).

% permutations with code similar to Lehmer code
perm_rlehmer([],[],[]).
perm_rlehmer([X|Xs],Zs,[K|Ks]):-
  perm_rlehmer(Xs,Ys,Ks),
  insk(X,Ys,Zs,K).
  
insk(X,Xs,[X|Xs],0).
insk(X,[Y|Xs],[Y|Ys],K1):-insk(X,Xs,Ys,K),K1 is K+1.

% fast computation of the sum of all factorials up to N!
sf(0,0).
sf(N,R1):-N>0,N1 is N-1,ndup(N1,1,Ds),rf([0|Ds],R),R1 is R+1.

sf(S):-nat(N),sf(N,S).

to_sf(N, K,N_M):-nat(X),sf(X,S),S>N,!,K is X-1,sf(K,M),N_M is N-M.

nat2perm(0,[]).
nat2perm(N,Ps):-to_sf(N, K,N_M),nth2perm(K,N_M,Ps).

perm2nat([],0).
perm2nat(Ps,N) :-perm2nth(Ps, Size,Nth),sf(Size,S),N is S+Nth.

nat2hfp --> default_ulimit(D),nat2hfp_(D).   
nat2hfp_(Ulimit) --> unrank_(Ulimit,nat2perm).
hfp2nat --> rank(perm2nat).

% generates integers From..To
ints_from(From,To,Is):-findall(I,between(From,To,I),Is).

% replicates X, N times
ndup(0, _,[]).
ndup(N,X,[X|Xs]):-N>0,N1 is N-1,ndup(N1,X,Xs).
  
zeros(N,Zs):-ndup(N,0,Zs).

mtranspose([],[]):-!.
mtranspose([Xs],Css):-!,to_columns(Xs,Css).
mtranspose([Xs|Xss],Css2):-!,
  mtranspose(Xss,Css1),
  to_columns(Xs,Css1,Css2).
	
to_columns([], []).
to_columns([X|Xs],[[X]|Zs]):-to_columns(Xs,Zs).

to_columns([],Css,Css).
to_columns([X|Xs],[Cs|Css1],[[X|Cs]|Css2]) :- to_columns(Xs,Css1,Css2).    

% conversion to list of digits in given base
to_base(Base,N,Bs):-to_base(N,Base,0,Bs).

to_base(N,R,_K,Bs):-N<R,Bs=[N].
to_base(N,R,K,[B|Bs]):-N>=R,
  B is N mod R, N1 is N//R,K1 is K+1,
  to_base(N1,R,K1,Bs).

% conversion from list of digits in given base
from_base(_Base,[],0).
from_base(Base,[X|Xs],N):-from_base(Base,Xs,R),N is X+R*Base.

% conversion to list of bits, right to left 
to_rbits(N,Bs):-to_base(2,N,Bs).

% conversion from list of bits, right to left 
from_rbits(Bs,N):-from_base(2,Bs,N).

% counting how many bits a number needs     
bitcount(N,K):-N=<1,K=1.
bitcount(N,K):-N>1,N1 is N>>1,bitcount(N1,K1),K is K1+1.

% finds the larges bitcount for a list
max_bitcount(Nss,L):-maplist(bitcount,Nss,Ls),max_list(Ls,L).  
  
% pads up to maxbits, if needed
to_maxbits(Maxbits,N,Rs):-
  to_base(2,N,Bs),length(Bs,L),ML is Maxbits-L,
  ndup(ML,0,Zs),append(Bs,Zs,Rs).

setShow(S):-gshow(S,"{,}"),nl.

gshow(0,[L,_C,R]):-put(L),put(R).
gshow(N,_):-integer(N),N>0,!,write(N).
gshow(Hs,[L,C,R]):-put(L),gshow_all(Hs,[L,C,R]),put(R).

gshow_all([],_).
gshow_all([H],LCR):-gshow(H,LCR).
gshow_all([H,G|Hs],[L,C,R]):-
  gshow(H,[L,C,R]),
  ([C]\=="~"->put(C);true),
  gshow_all([G|Hs],[L,C,R]).

c:-['../src/pFun.pro'].

