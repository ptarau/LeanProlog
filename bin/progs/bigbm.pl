% factoradics of N, right to left
fr(0,[0]).
fr(N,R):-N>0,fr1(1,N,R).
   
fr1(_,0,[]).
fr1(J,K,[KMJ|Rs]):-K>0,
  KMJ is K mod J,
  J1 is J+1,
  KDJ is K // J,
  fr1(J1,KDJ,Rs).

fl(N,Ds):-fr(N,Rs),reverse(Rs,Ds).

lf(Ls,S):-length(Ls,K),K1 is K-1,lf(K1,_,S,Ls,[]).

% from list of digits of factoradics, back to decimals
lf(0,1,0)-->[0].
lf(K,N,S)-->[D],{K>0,K1 is K-1},lf(K1,N1,S1),{N is K*N1,S is S1+D*N}.

% from factoradics back to big integers
rf(Ls,S):-reverse(Ls,Rs),lf(Rs,S).

fact(N,R):-fact(N,1,R).

fact(0,N,N).
fact(N,R1,R3):-N>0,R2 is R1*N,N1 is N-1,fact(N1,R2,R3).

fbm1a:-symgc_off,fbm1,symgc_on,symgc.

fbm1:-
  time(fact(20000,_),T),write(T),nl,fail.
fbm1.

swibm1:-
  time(fact(20000,_)),fail.
swibm1.

frbmn(N,M):-fr(N,Ds),rf(Ds,M).

frbm(X):-N is 1234^X,frbmn(N,M),N=:=M.

fbm2a:-symgc_off,fbm2,symgc_on,symgc.

fbm2:-
  N is 5678,
  time(frbm(N),T),write(T),nl,fail.
fbm2.

swibm2:-
  N is 5678,
  time(frbm(N)),fail.
swibm2.

