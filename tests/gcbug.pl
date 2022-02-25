go:-bug.


bug:-
  for(I,1,100000),
  assert(a(I)),
  fail.
bug:-
  findall(I,a(I),Is),
  length(Is,L),
  println(done(L)).

bug1:-
  for(I,1,10000),
  assert(a(I)),
  fail.
bug1:-
  traceln(here),
  findall(I,a(I),Is),
  traceln(there),
  length(Is,L),
  println(done(L)).


bug2:-
  index(a(1)),
  for(I,1,10000),
  assert(a(I)),
  fail.
bug2:-
  traceln(here),
  findall(1,b(_),_).

b(X):-a(X).
b(X):-numlist(1,10000,Xs),member(X,Xs),a(X).

