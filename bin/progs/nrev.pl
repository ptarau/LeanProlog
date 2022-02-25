app([],Ys,Ys).
app([A|Xs],Ys,[A|Zs]):-app(Xs,Ys,Zs).

nrev([],[]).
nrev([X|Xs],Zs):-nrev(Xs,Ys),app(Ys,[X],Zs).

full_range(It,L):- irange(_,1,It),nrev(L,_),fail.
full_range(_,_).

dummypred(_,_).

empty_range(It,L):-irange(_,1,It),dummypred(L,_),fail.
empty_range(_,_).

irange(Min,Min,Max):-Min=<Max.
irange(I,Min,Max):-Min<Max,Min1 is Min+1,irange(I,Min1,Max).

integers([],I,I):-!.
integers([I0|L],I0,I):-I0<I,I1 is I0+1,integers(L,I1,I).

lgo(nrev(iter(It),len(Len),time(T))):-
  Len=100,
	It=3000,
  tbm(It,Len,T).

tbm(It,Len,MsTime):-
	integers(L,0,Len),
	timer(T0),
	empty_range(It,L),
	timer(T1),
	full_range(It,L),
	timer(T2),
	T21 is T2-T1,
	T10 is T1-T0,
	MsTime is T21-T10.

bm(It,Len,Time,Lips):-
  tbm(It,Len,_), % empty run - to possibly trigger JIT/HotSpot
  tbm(It,Len,Time),
	L1 is Len+1,
	L2 is L1+1,
	LI10 is L1*L2,
	LI is LI10/2,
	LIs is It*LI,
	Lips is LIs/Time.


go3(Mes,Len,It):-
	bm(Len,It,T,L),
	nl,write(Mes=[klips=L,time=T,length=Len,iterations=It]),nl.

big:-big1('BMARK_brev_1000_times_100_elem_nrev:').

mid:-
	Len=50,
	It=1000,
  Mes=nrev(it(It),len(Len)),
	go3(Mes,Len,It).

big1(Mes):-
	Len=100,
	It=1000,
	go3(Mes,Len,It).

small:-small1('BMARK_brev_100_times_30_elem_nrev:').

small1(Mes):-
	Len=30,
	It=100,
	go3(Mes,Len,It).
	
%timer(T):-cputime(T0),*(T0,1000,T).

timer(T):-statistics(runtime,[T,_]).

go:-big.
