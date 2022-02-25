nfact(0,1).
nfact(N,R):- N>0,N1 is N-1,nfact(N1,R1),R is N*R1.

nfact20(N,R):-N2 is 2*N,nfact(N2,R).
nfact21(N,R):-N2 is 2*N+1,nfact(N2,R).

nfactA(N,R):-nfact20(N,R);nfact21(N,R).
nfactB(N,R):-nfact21(N,R);nfact20(N,R).


nf20(X,[RA,RB,RA1,RB1]):-
  new_engine(A,nfactA(X,A),EA),
  get(EA,RA),
  %println(RA),

  new_engine(B,nfactA(X,B),EB),
  get(EB,RB),
  %println(RB),

  %symgc,

  new_engine(A1,nfactA(X,A1),EA1),
  get(EA1,RA1),
  %println(RA1),

  %symgc,

  new_engine(B1,nfactA(X,B1),EB1),
  get(EB1,RB1),
  %println(RB1),

  %symgc,

  stop(EA),stop(EA1),stop(EB),stop(EB1),

  %symgc,

  true.

go:-nf20(10000,_),fail.


