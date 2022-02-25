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
  Th=8,
  Len=100,
	It=3000,
  tbm(Th,It,Len,T).

tbm(Th,It,Len,MsTime):-
	integers(L,0,Len),
  findall((ignore:-full_range(It,L)),between(1,Th,_),XGs),
	timer(T0),
	empty_range(It,L),
	timer(T1),
  multi_all(XGs,_),
	timer(T2),
	T21 is T2-T1,
	T10 is T1-T0,
	MsTime is T21-T10.

bm(Th,It,Len,Time,Lips):-
  tbm(Th,It,Len,_), % empty run - to possibly trigger JIT/HotSpot
  tbm(Th,It,Len,MTime),
  Time is MTime/Th,
	L1 is Len+1,
	L2 is L1+1,
	LI10 is L1*L2,
	LI is LI10/2,
	LIs is It*LI,
	Lips is LIs/Time.


go3(Th,Mes,Len,It):-
	bm(Th,It,Len,T,L),
	nl,write(Mes=[threads=Th,klips=L,time=T,length=Len,iterations=It]),nl.

big:-big(8).

big(Th):-big1(Th,'BMARK_brev_100_times_3000_elem_nrev:').

big1(Th,Mes):-
	Len=100,
	It=3000,
	go3(Th,Mes,Len,It).

small:-small(8).

small(Th):-small1(Th,'BMARK_brev_100_times_30_elem_nrev:').

small1(Th,Mes):-
	Len=30,
	It=100,
	go3(Th,Mes,Len,It).
	
%timer(T):-cputime(T0),*(T0,1000,T).

timer(T):-statistics(runtime,[T,_]).
