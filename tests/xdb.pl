
    
db_test1:-
  Db=mydb,Db1=otherdb,
  index(a(1,0,0)),
  index(a(1)),
  db_assert(Db,a([],Ys,Ys)),
  db_assert(Db,(
    a([X|Xs],Ys,[X|Zs]):-a(Xs,Ys,Zs)
  )),
   
  db_assert(Db1,a(1)),
  db_assert(Db1,a(2)),

  db_listing(Db1),

  db_listing(Db),
  
  db_retractall(Db,a([_|_],[_|_],_)),
  db_listing(Db),
  
  xdb_size(S),
  xdb_show,
  println(xdb_size=S).

/*
db_test2:-
  db_consult('$','src/xdb/xdb.pl'),
  db_listing(_),
  xdb_size(S),println
*/

db_test:-
  index(nrev(1,0)),
  index(app(1,0,0)),
  db_consult('$','src/xdb/nrev.pl'),
  db_listing,
  xdb_size(S),println(size=S),
  do_goal('$',small).

xconsult:-
  time(consult('progs/wtop.pro')).

xsave:-
  xconsult,
  time(xdb_save('wtop.px')).
  
xload:-  
   println('bug due to ConsoleIO'),
   time(xdb_load('wtop.px')).
   
xbug:-
  xload,
  abolish(g/2). 


/*
% Canonical indexable form API:
% records of the form: K:H:-B with indexing only on K which should be ground

xadd(Db,KC):-
  to_clause(KC,(K:H:-B)), % K could be [X|Xs] !!!
  xdb_put(Db,K,(K:H:-B),_).

xget(Db,K:H,B):-xdb_get(Db,K,(K:H:-B),_).
  
xrm(Db,K:H,B):-xdb_get(Db,K,(K:H:-B),Ref),xdb_remove(Db,K,Ref,Ok),Ok=1.

xall(Db,K,(H:-B)):-xdb_get(Db,K,(K:H,B),_).

xlst:-xlst(_Db).

xlst(Db):-xlst(Db,_K).

xlst(Db,K):-
  foreach(
    xall(Db,K,C),
    portray_clause(C)
  ),
  nl.
  

% db_assert API



     
% ground assert/retract API
    
xassert(C0):-
  to_clause(C0,C),
  C=(H:-_),
  xdb_put(H,C,_).

xclause(H,B,Ref):-xdb_get(H,(H:-B),Ref).

xclause(H,B):-xdb_get(H,(H:-B),_).

xretract(H):-
  xdb_get(H,(H:-_),Ref),
  xdb_remove(H,Ref,Ok),Ok=1.

xretractall(H):-xretract(H),fail.
xretractall(_).

xlisting:-xlisting1(_).
   
xlisting(F/N):-
  functor(H,F,N),
  xlisting1(H).

xlisting1(H):-  
  foreach(
    xclause(H,B),
    portray_clause((H:-B))
  ).
  

  

file2saved(InF,SavedF):-
  findall(T,term_of(InF,T),Ts),
  shm_put(SavedF,Ts).
  
saved2terms(SavedF,Ts):-
  shm_get(SavedF,Ts).
    

pl2plx(InF,SavedF):-
  findall(T,term_of(InF,T),Ts),
  to_file(SavedF,Ts).
  
plx2mem(SavedF,Ts):-
  from_file(SavedF,Ts).
    
xftest1:-
  delete_file(boo),
  pl2plx('src/xdb/xbm.pl',boo),
  plx2mem(boo,Ts),
  xlen(Ts,L),
  println(len=L).

xftest:-
  pl2plx('progs/wtop.pro','wtop.plx'),
  plx2mem('wtop.plx',Ts),
  xlen(Ts,L),
  println(len=L).
  
wnsave:-
  time(file2saved('progs/wtop.pro',wtop)),
  println(saved).

wnload:-  
  time(
    saved2terms(wtop,Ts),
    T1
  ),
  println(loaded),
  time((
    xlen(Ts,L),
    println(len(L)),
    fail
  ),T2),
  println(done(load=T1,count=T2)).
    
    
xnsave:-
  time(pl2plx('progs/wtop.pro',wtop)),
  println(saved).

xnload:-  
  time(
    plx2mem('wtop.plx',Ts),
    T1
  ),
  println(loaded),
  time((
    xlen(Ts,L),
    println(len(L)),
    fail
  ),T2),
  println(done(load=T1,count=T2)).
 
 
xconsfacts(F0):-
  xdb_clear,
  atom_concat(F0,'.pro',F),
  term_of(F,T),
  xdb_put(T,0,_Ref),
  fail.
xconsfacts(F0):-
  xdb_size(T),
  println(total_size(T)),
  xdb_indexer(I),
  shm_put(F0,I).  
        
        
yconsfacts(F0):-
  xdb_clear,
  atom_concat(F0,'.plx',F),
  plx2mem(F,T),
  xdb_put(T,0,_Ref),
  fail.
yconsfacts(F0):-
  xdb_size(T),
  println(total_size(T)),
  atom_concat(F0,'.px',F),
  xdb_indexer(I),
  to_file(F,I).  
*/
     
     
           