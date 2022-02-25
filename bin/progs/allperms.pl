allperms([],[[]]).
allperms([X|Xs],Perms2):-
       	allperms(Xs,Perms1),
        extendperms(Perms1,X,Perms2).

extendperms([],_,[]).
extendperms([Perm|Perms1],X,[[X|Perm]|Perms3]):-
	extendperms(Perms1,X,Perms2),
	insert_one_item(Perm,X,[],Perms2,Perms3).

insert_one_item([],_,_,Perms,Perms).
insert_one_item([Y|Ys],X,Acc,Perms1,[Zs|Perms2]):-
       	revapp(Acc,[Y,X|Ys],Zs),
        insert_one_item(Ys,X,[Y|Acc],Perms1,Perms2).

revapp([],Acc,Acc).
revapp([X|Xs],Acc,Zs):-
       revapp(Xs,[X|Acc],Zs).

nats(Max,Max,[Max]):-!.
nats(Curr,Max,[Curr|Ns]):-
	Curr<Max,
	Curr1 is Curr+1,
	nats(Curr1,Max,Ns).

permute([],[]).
permute([X|Xs],Zs):-
	permute(Xs,Ys),
	ins(X,Ys,Zs).

ins(X,Ys,[X|Ys]).
ins(X,[Y|Ys],[Y|Zs]):-
	ins(X,Ys,Zs).

g0(N):-nats(1,N,Ns),permute(Ns,_),fail.
g0(_).

g1(N,Ps):-nats(1,N,Ns),allperms(Ns,Ps).

g2(N,Ps):-nats(1,N,Ns),findall(P,permute(Ns,P),Ps).

g3(N,Qs):-nats(1,N,Ns),allperms(Ns,Ps),maplist(copy_term,Ps,Qs).

g4(N):-nats(1,N,Ns),allperms(Ns,Ps),member(P,Ps),copy_term(P,_),fail.
g4(_).

g5(N,Ps):-nats(1,N,Ns),alt_findall(P,permute(Ns,P),Ps).

test(Mes,X):-
  Iter=10,
  statistics(runtime,[T1,_]),
	(between(1,Iter,_),X,fail;true),
  statistics(runtime,[T2,_]),
  T is T2-T1,
  write('++++>'),write(Mes),write(' time='),write(T),nl.

t0:-test('perms:',g0(8)).

t1:-test('allperms:',g1(8,_)).

t2:-test('allperms with findall:',g2(8,_)).

t3:-test('allperms + maplist + copyterm:',g3(8,_)).

t4:-test('allperms + copyterm:',g4(8)).

t5:-test('allperms with alt_findall:',g5(8,_)).

go:-(nl,t0,fail;t1,fail;t2,fail;nl).

