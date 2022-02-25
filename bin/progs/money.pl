go:-
  cputime(T1),
  puzzle(Show,[0,1,2,3,4,5,6,7,8,9],_),
  cputime(T2),
  T is T2-T1,
  Show,
  write('BMARK_money:'=time(T)),nl.


puzzle(show(S,E,N,D,M,O,R,Y))-->
  add_digits(D,E,Y,  0,R1),
  add_digits(N,R,E, R1,R2),
  add_digits(E,O,N, R2,R3),
  add_digits(S,M,O, R3,
                 M),
  {S>0,M>0}.

digit(X)-->{integer(X)},!.
digit(X)-->sel(X).

add_digits(C1,C2,Res,R1,R2)-->
   digit(C1),
   digit(C2),
   digit(Res),
   {add_with_carry(C1,C2,R1,Res,R2)}.

add_with_carry(C1,C2,R1,Res,R2):-
    S is (C1+C2)+R1,
    Res is S mod 10,
    R2 is S // 10.

sel(X,[X|Xs],Xs).
sel(X,[Y|Xs],[Y|Ys]):-sel(X,Xs,Ys).

show(S,E,N,D,M,O,R,Y):-
( write('  '),
  write([S,E,N,D]),
  write('+'),nl,
  write('  '),
  write([M,O,R,E]),
  write('='),nl,
  write([M,O,N,E,Y]),nl,
  fail
; nl
).

