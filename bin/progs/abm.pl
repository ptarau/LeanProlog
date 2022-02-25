% db

abm(N):-
  eb_clear,
  assert(a(0)),
  for(I,0,N),
  J is I+1,
  assert(
    (a(J):-a(I))
  ),
  fail.
abm(_).  

abm:-for(I,1,10),ab1,fail.
abm.

ab1:-time(abm(100000)).
ab2:-time(abm(200000)).   

% external dict

gbm(N):-
  for(I,0,N),
  J is I+1,
  I<==J,
  fail.
gbm(_).


gb1:-time(gbm(100000)).
gb2:-time(gbm(200000)).   

% local dict

lbm(N):-
  for(I,0,N),
  J is I+1,
  x<=J,
  fail.
lbm(_).


lb1:-time(lbm(100000)).
lb2:-time(lbm(200000)). 

% engines cleared

ebm:-time(ebm(100000)).

ebm(N):-
  for(I,0,N),
  findall(X,(current_engine(_),member(X,[1,2])),_),
  fail.
ebm(N).
  
% big ints cleared 10

xbm:-time(xbm(100000)).
  
xbm(N):-
  for(I,0,N),
  J is I*I,
  K is J*J,
  fail.
xbm(_).
  