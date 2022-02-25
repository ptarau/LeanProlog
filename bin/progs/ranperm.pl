
% factoradics of N, right to left
fr(0,[0]).
fr(N,R):-N>0,fr1(1,N,R).
   
fr1(_,0,[]).
fr1(J,K,[KMJ|Rs]):-K>0,
  KMJ is K mod J,J1 is J+1,KDJ is K // J,
  fr1(J1,KDJ,Rs).

fl(N,Ds):-fr(N,Rs),reverse(Rs,Ds).

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

% generates integers From..To
ints_from(From,To,Is):-findall(I,between(From,To,I),Is).

% replicates X, N times
ndup(0, _,[]).
ndup(N,X,[X|Xs]):-N>0,N1 is N-1,ndup(N1,X,Xs).

ranperm(N,Ps):-
  ints_from(1,N,Ns),
  prodlist(Ns,FactN),
  random(FactN,K),
  nth2perm(N,K,Ps).
  
ranperm(Start,N,Ps):-
  ranperm(N,Ps0),
  maplist(+(Start),Ps0,Ps).
  
make_pair(X,Y,X-Y).

pair_snd(_-Y,Y).
  
ranpermute(Ps,Qs):-
  length(Ps,L),
  ranperm(L,Rs),
  maplist(make_pair,Rs,Ps,RPs),
  keysort(RPs,SortedPairs),
  maplist(pair_snd,SortedPairs,Qs).