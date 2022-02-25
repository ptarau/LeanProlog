% Variant of a program by T. Fruhwrith
:-[ranperm].

bm:-allbm(11).

allbm(N):-
  time(allqueens(N)).
  
allqueens(N):-queens(N,_),fail.
allqueens(_).


go:-go(_queen),write(x),fail.
go:-nl.

go(Qs):-queens(8,Qs).

queens(N,Ps):-
  gen_places(N,Ps),
  gen_queens(N,Qs),
  place_queens(Qs,Ps,_,_).

% at each step inc Us and dec Ds
place_queens([],_,_,_).
place_queens([I|Is],Cs,Us,[_|Ds]):-
  place_queens(Is,Cs,[_|Us],Ds),
  place_queen(I,Cs,Us,Ds).

place_queen(I,[I|_],[I|_],[I|_]).
place_queen(I,[_|Cs],[_|Us],[_|Ds]):-
  place_queen(I,Cs,Us,Ds).

gen_places(Max,Ps):-
  findall(_,for(_,1,Max),Ps).

gen_queens(Max,Qs):-
  findall(Q,for(Q,1,Max),Qs).

ran_queens(N,Ps):-
  gen_places(N,Ps),
  ranperm(1,N,Qs),
  %gen_queens(N,Qs),
  place_queens(Qs,Ps,_,_).
  
fast_first_queen(Threads,N,Qs):-
  ran_queens_goal(Threads,N, XGs),
  multi_first(XGs,Qs).

ran_queens_goal(Threads,N,XGs):-
  XG=(Ps:-ran_queens(N,Ps)),
  findall(XG,for(_,1,Threads),XGs).

qbm(Threads,N):-
  %N=20,
  time(queens(N,Qs),T0),
  time(ran_queens(N,RQs),T1),
  time(fast_first_queen(Threads,N,FQs),T2),
  println(T0+T1+T2),
  maplist(println,[Qs,RQs,FQs]).  