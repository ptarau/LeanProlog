empty_for(N):-
  between(1,N,_),
  fail.
empty_for(_).
 
 
full_for(N,G):-
  between(1,N,_),
  G,
  fail.
full_for(_,_).

gdb(G):-assert(G),retract1(G).
geb(G):-eb_assert(G),eb_retract1(G).

test2(N,TD+TE):-
  G=a(b),
  GDB=gdb(G),
  GEB=geb(G),
  time(empty_for(N),T0),
  time(full_for(N,GDB),T1),
  open_eb,time(full_for(N,GEB),T2),eb_abolish(a,1),close_eb,
  abolish(a,1),
  TDB is T1-T0,
  TEB is T2-T1,
  time(empty_for(N),TT0),
  open_eb,time(full_for(N,GEB),TT1),  eb_abolish(a,1),close_eb,
  time(full_for(N,GDB),TT2),
  abolish(a,1),
  TTEB is TT1-TT0,
  TTDB is TT2-TT1,
  TD is TDB+TTDB,
  TE is TEB+TTEB.
  
 
 go(N):-test2(N,T),println('!!!!!!!!!!!!!'(T)),fail.
 go(_):-stats.
 
 
 go:-go(100000).
  
