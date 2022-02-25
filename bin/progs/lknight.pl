go:-go(5).

go(N):-
	make_board(N,Board,NbMoves),
	xtime(knight(NbMoves,1,1,Board),T),!,
	show_board(N,Board),
	write(time(T)),nl.

make_board(N,Board,M):-
	length(L,N),
	findall(L,nth_member1(_,L,_),Board),
	M is N*N.

val(I,J,V,Board):-nth_member1(L,Board,I),nth_member1(V,L,J),!.

nth_member1(X,Xs,I):-nth1(I,Xs,X).

knight(0,_,_,_) :- !.
knight(K,A,B,Board) :-
  K1 is K-1,
  val(A,B,K,Board),
  move(Dx,Dy),
  step(K1,A,B,Dx,Dy,Board).

step(K1,A,B,Dx,Dy,Board):-
    C is A + Dx,
    D is B + Dy,
    knight(K1,C,D,Board).

show_board(N,Board):-
			nth_member(L,Board,_I),nth_member(V,L,J),
			write(' '),
			X is 1-V // 10, tab(X),
			write(V),
			(J=:=N->nl;true),
	fail.
show_board(_,_):-nl.

move( 2, 1).
move( 2,-1).
move(-2, 1).
move(-2,-1).
move( 1, 2).
move(-1, 2).
move( 1,-2).
move(-1,-2).



xtime(G,T):-
  statistics(runtime,[T1,_]),
  (G->true;true),
  statistics(runtime,[T2,_]),
  T is T2-T1.
  
  