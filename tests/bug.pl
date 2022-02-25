buggyboo(99).

runbug :-
  runbug(10).
 
runbug(0):-!.
runbug(N):-N>0,
  N1 is N-1,
  findall(X,buggyboo(X),_),
  !,
  runbug(N1).  