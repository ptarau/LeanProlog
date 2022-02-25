bug:-
  X=1+1,
  T is X+1,
  println(T).

nobug:-
 X=1+1,
 expr(X+1,T),
 println(T).
