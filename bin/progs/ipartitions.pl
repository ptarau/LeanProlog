% backtracking integer partition iterator
 
integer_partition_of(N,Ps):-
  nats(N,Is),
  split_to_sum(N,Is,Ps).

split_to_sum(0,_,[]).
split_to_sum(N,[K|Ks],R):-
  N>0,
  sum_choice(N,K,Ks,R).

sum_choice(N,K,Ks,[K|R]):-
  NK is N-K,
  split_to_sum(NK,[K|Ks],R).
sum_choice(N,_,Ks,R):-
  split_to_sum(N,Ks,R).

nats(1,[1]).
nats(N,[N|Ns]):-
  N>1,
  N1 is N-1,
  nats(N1,Ns).

count_partitions(N,R):-
  new_engine(1,integer_partition_of(N,_),Engine),
  efoldl(Engine,'+',0,R).
  

go:-go(50).

go(N):-statistics(runtime,[T1,_]),
      (integer_partition_of(N,_),fail;true),
      statistics(runtime,[T2,_]),
      T is T2-T1,
      write(time(T)),nl.
     
      
 % experiments
 
decreasing_partition(N,Ns):-
  integer_partition_of(N,Ns),
  is_decreasing(Ns).
  
is_decreasing([]).
is_decreasing([X|Xs]):-is_decreasing1(Xs,X).

is_decreasing1([],_).
is_decreasing1([Y|Ys],X):-Y<X,is_decreasing1(Ys,Y).

best_partition(N,Ns):-
  findall(Ns,decreasing_partition(N,Ns),Dss),
  sort(Dss,[Ds|_]),
  sort(Ds,Ns).


  
set2fun(Xs,Ns):-sort(Xs,Is),set2fun(Is,-1,Ns).

set2fun([],_,[]).
set2fun([X|Xs],Y,[A|As]):-A is (X-Y)-1,set2fun(Xs,X,As).

fun2set(Ns,Xs):-fun2set(Ns,-1,Xs).

fun2set([],_,[]).
fun2set([X|Xs],Y,[A|As]):-A is (X+Y)+1,fun2set(Xs,A,As).

mset2fun(Xs,Ns):-msort(Xs,Is),mset2fun(Is,0,Ns).

mset2fun([],_,[]).
mset2fun([X|Xs],Y,[A|As]):-A is (X-Y),mset2fun(Xs,X,As).

fun2mset(Ns,Xs):-fun2mset(Ns,0,Xs).

fun2mset([],_,[]).
fun2mset([X|Xs],Y,[A|As]):-A is (X+Y),fun2mset(Xs,A,As).

set2mset(Xs,Ms):-set2mset(Xs,1,Ms).

set2mset([],_,[]).
set2mset([N|Ns],K,[M|Ms]):-K1 is K+1,M is N-K,set2mset(Ns,K1,Ms).

mset2set(Ms,Ns):-mset2set(Ms,1,Ns).
  
mset2set([],_,[]).
mset2set([M|Ms],K,[N|Ns]):-K1 is K+1,N is M+K,mset2set(Ms,K1,Ns).
  

munpair(N,I,J):-
  to_mset(N,Ms),
  countx(0,Ms,I),
  countx(1,Ms,J).
  
countx(X,Ns,L):-findall(X,member(X,Ns),Xs),length(Xs,L).  
  
inc_with(K,X,Y):-Y is X+K.

to_mset(0,[]).
to_mset(N,Ms):-
  best_partition(N,Ns),
  set2mset(Ns,Ms).
  
to_pmset(N,Ps):-inc_with((-1),N,M),to_mset(M,Ms),maplist(inc_with(2),Ms,Ps).

show_msets(Max):-
  between(0,Max,I),
  to_mset(I,Ms),
  write(I+Ms),nl,
  fail
; nl.

show_pmsets(Max):-
  between(1,Max,I),
  to_pmset(I,Ms),
  write(I+Ms),nl,
  fail
; nl.


show_munpair(Max):-
  between(0,Max,N),
  munpair(N,I,J),
  mpair(I,J,M),
  write((N=M)=(I,J)),nl,
  fail
; nl.



mpair(I,J,N):-
  length(Xs,I),
  maplist(=(0),Xs),
  length(Ys,J),
  maplist(=(1),Ys),
  append(Xs,Ys,Zs),
  mset2set(Zs,Us),
  sumlist(Us,N).
  
% faster than munpair - still O(N^3)?  
munpair_(N,I,J):-
  M is N-1,
  between(0,M,I),
  between(0,M,J),
  mpair(I,J,N),
  !.
  
mptest(N):-
  show_unpair(munpair_,N).
  
