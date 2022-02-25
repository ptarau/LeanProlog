go:-
	write('use bp -h4000'),nl,
	list(L),augment(L,L3),augment(L3,L9),augment(L9,L27),
	xtime(T1),
		qsort(L27,_Sorted,[]),
	xtime(T2),
        T is T2-T1,
	write(time=T),nl.

go1:-L=[3,4,1,2,1,2,5,1,3,0,9,7],qsort(L,R,[]),write(L-R),nl.

on(X,[X|_]).
on(X,[_|Xs]):-on(X,Xs).

augment(L,R):-findall(X,(on(Y,L),(X is Y+1;Y=X;X is Y-1)),R).

list([27,74,17,33,94,18,46,83,65, 2,
		32,53,28,85,99,47,28,82, 6,11,
		55,29,39,81,90,37,10, 0,66,51,
		 7,21,85,27,31,63,75, 4,95,99,
		11,28,61,74,18,92,40,53,59, 8]).

qsort(L,R):-qsort(L,R,[]).

qsort([],S,S).
qsort([Y|L],S1,S3) :- qpartition(L,Y,S1,S3).

qpartition([],Y,[Y|S],S).
qpartition([X|L],Y,S1,S3) :- 
	qpartition1(L,Y,L1,L2,X),
	qsort(L1,S1,[Y|S2]),
	qsort(L2,S2,S3).

qpartition1(L,Y,[X|L1],L2,X) :-
	X =< Y,!,
	qpartition2(L,Y,L1,L2).
qpartition1(L,Y,L1,[X|L2],X) :-
	qpartition2(L,Y,L1,L2).

qpartition2([],_,[],[]).
qpartition2([X|L],Y,L1,L2) :- qpartition1(L,Y,L1,L2,X).

xtime(T):-statistics(runtime,[T,_]).
