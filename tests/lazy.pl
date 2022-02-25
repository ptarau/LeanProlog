


go:-go(_,_).

go-->
  lazy_set_state(I,for(I,1,10)),
  lazy_get_state(L0),
  lazy_head(X),
  lazy_get_state(L1),
  {println(l1=L1)},
  {println(l0=L0)},
  lazy_drop(2),
  lazy_head(Y),
  lazy_tail,
  lazy_head(Z),
  lazy_get_state(L),
  {println([X,Y,Z]),
  println(end=L)}.

go4:-
  lazy_findall(I,for(I,1,6),L0),
  println(l0=L0),
  lazy_drop(3,L0,L1),
  println(l0=L0),
  println(l1=L1),  
  nl,
  lazy_take(5,XXs,L1,L2),
  println(xxs=XXs),
  println(l1=L0),
  println(l2=L2),  
  fail.

go3:-
  lazy_findall(I,for(I,1,10),L0),
  println(l0=L0),
  lazy_take(3,Xs,L0,L1),
  println(xs=Xs),
  println(l0=L0),
  println(l1=L1),  
  nl,
  lazy_take(5,XXs,L1,L2),
  println(xxs=XXs),
  println(l1=L0),
  println(l2=L2),  
  fail.

go2:-
  lazy_findall(I,for(I,1,10),L0),
  println(l0=L0),
  lazy_split2(X,L0,L1),
  println(x=X),
  println(l0=L0),
  println(l1=L1),  
  lazy_split2(Y,L1,L2),
  println(y=Y),
  println(l1=L1),
  println(l2=L2),  
  fail.


go1:-
  lazy_findall(I,for(I,1,10),L0),
  println(L0),
  lazy_force(L0,L1),
  println(L1),
  lazy_force(L1,L2),
  println(L2),
  fail.
