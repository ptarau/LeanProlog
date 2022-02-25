% server tests

xprintln(_).

mbgtest0:-mbg([(R:-forx(I,1,5),sleep(1),println(I),fail;R=ok)],R),println(R).
 
% tests - better, simpler than mbg !!!

forsym(S,From,To):-for(I,From,To),atom_concat(sym_,I,S).

forx(Term,From,To):-
  for(I,From,To),atom_concat(sym_,I,F),
  N is (1+abs(To-I))/(1+abs(To-From)),
  atom_concat(F,'_arg',A),
  Term=..[F,f(X,Y),A,N,g(Y,X)].

forxs(N,Xs):-findall(X,forx(X,1,N),Xs).
 
forxss(M,Xss):-
  findall(Xs,(for(N,1,M),forxs(N,Xs)),Xss).
  
  
mxtest:-
  K=50,M=100,
  make_mxtest_workers(K,M,XGs),
  multi_first(XGs,First),
  length(First,L),
  println(length(L)).
   
make_mxtest_workers(K,M,XGs):-
  G=forxss(I,Xss),
  findall((Xss:-G),for(I,K,M),XGs).
  
  
xti:-
  new_task_group(TG),
  foreach(
   for(I,1,10),
   new_task(TG,t(J,I),forx(J,1,I))
  ),
  foreach(
    run_tasks(TG,X),
    traceln(X)
  ),
  stop_task_group(TG).
   
xt(R):-
  Max=10,
  add_task(a(X),for(X,1,Max),TG),
  add_task(b(X),for(X,1,Max),TG),
  add_task(c(X),for(X,1,Max),TG),
  add_task(d(X),for(X,1,Max),TG),
  run_tasks(TG,R).
     
     
xt1(R+Z):-
  max<==10,
  Max=10,
  hub(H),
  aaa<=999,
  call_at(H,
    [
      (a(X):-atom_concat(a,1,A1),A1<==111,for(X,1,Max)),
      (b(X):-aaa<==222,for(X,1,Max)),
      (c(X):-aaa<==333,for(X,1,Max)),
      (d(X):-aaa<==444,max==>M,for(X,1,M))
    ],
  R),
  aaa==>Z.

mstest:-mstest(100).

mstest(M):-
  bg(s_server,Hub),
  s_wait,
  ( for(I,1,M),
      traceln(forxss(I)),
      bg(forxss(I,_),Hub),
      fail
  ; true
  ),
  stop(Hub).

 
 
mstest1:-
  bg(s_server),
  s_wait,
  for(I,1,10),
    bg(mstask(I)),
    %mstask(I),
  fail.
mstest1.

mstask:-mstask(100).
    
mstask(I):-
  for(J,1,1000),
  sleep_ms(10),
  s_call(X is I+J),
  traceln(X),
  fail.
mstask(I):-traceln(I).
  
  
istest:-istest(25).
 
istest(N):-
  i_server(S),
  new_task_group(TG),
  foreach(
    between(1,N,I),
    new_task(TG,I,istask(S,N,I))
  ),
  traceln(starting_tasks),
  foreach(
    run_tasks(TG,I),
    traceln(done(I))
  ),
  traceln(finished_tasks),
  stop_task_group(TG),
  i_halt(S),
  stats.

    
istask(S,N,I):-
  traceln(enter(I)),
  for(J,1,N),
  K is I+J,
  i_call(S,forxss(K,_Xs)),
  fail.
istask(_,_,I):-traceln(exit(I)).
    

% fails with Clone=0
mbgtest:-mbgtest(25).
 
mbgtest(N):-
  new_task_group(TG),
  foreach(
    between(1,N,I),
    new_task(TG,I,mbgtest(N,I))
  ),
  traceln(starting_tasks),
  foreach(
    run_tasks(TG,I),
    traceln(done(I))
  ),
  traceln(finished_tasks),
  stop_task_group(TG),
  stats.

    
mbgtest(N,I):-
  traceln(enter(I)),
  for(J,1,N),
  K is I+J,
  forxss(K,_Xs),
  fail.
mbgtest(_,I):-
  traceln(exit(I)).
    

bgtest0:-
  % fails with Clone=0
  for(I,1,100),
  bg(forxss(I,_)),
  traceln(I),
  fail
; nl.

      

iserver2:-
  i_server(S),
  i_call(S,i_server(T)),
  (for(I,1,100),
  % bg brakes it as i_server uses Clone=0
  bg(i_call(S,forxss(I,_))),
  i_call(T,forxss(I,_)),
  traceln(I),
  fail
  ;traceln(done)
  ).
  
lstt:-bg(s_server),s_wait,stt(5),s_stop.


stt:-stt(5).

% start server in different window
stt(N):-
  % s_server(S),
  new_task_group(TG),
  foreach(
    between(1,N,I),
    new_task(TG,I,sttask(N,I))
  ),
  traceln(starting_tasks),
  sleep_ms(100),
  foreach(
    run_tasks(TG,I),
    traceln(done(I))
  ),
  traceln(finished_tasks),
  stop_task_group(TG),
  symgc,stats.

    
sttask(N,I):-
  traceln(enter(I)),
  for(J,1,N),
  %sleep_ms(50),
  s_call(traceln(t(I,J))),
  %traceln(X),
  fail.
sttask(_,I):-traceln(exit(I)).    

    
    
% TESTS xtests

serpar:-
 bg(s_server(5555)),bg(s_server(6666)).
 

clipar:-
 add_task(R,ctask(5555,a,1000,R),TG),
 add_task(R,ctask(6666,b,1000,R),TG),
 foreach(
   run_tasks(TG,X),
   println(X)
 ).

ctask(P,F,N,R):-for(I,1,N),s_call(P,println(x(F,I))),fail;R=done(F,N).

i_bug2:-for(_,1,50),i_client_test(1000,1000),fail.

i_bug1:-i_client_test(50000,1000),fail.

i_bug:-
   for(_,1,1000),statistics,i_client_test,fail.
   
% socket based interprocess communication

% assumes s_server. runs in another window

s_client_test:-
  s_connection(C),
    s_run_at(C,println(hello)),
    %traceln(back),
    s_run_at(C,println(bye)),
    s_run_at(C,findall(X,member(X,[a,b,c]),Xs)),
  s_disconnect(C),
  foreach(member(X,Xs),println(X)).

s_client_test(N,Len):-
  s_connection(C),
  time(lsock_test1(C,N,Len)),
  s_disconnect(C).

lsock_test1(C,N,Len):-
  numlist(1,Len,Xs),
  for(_,1,N),
    s_run_at(C,length(Xs,_)),
    %println(Xs+L),
  fail.
lsock_test1(_,N,Len):-
  println(done(N,len(Len))).  
  
  

% inner sockets - thread to thread communication  
  
i_client_test:-
 i_server(println(hello_from_server),S),
 i_test1(S,aaa),
 i_test1(S,bbb),
 i_connection(S,C),
 i_stop(C).
 
i_test1(S,X):- 
 i_connection(S,C),

 i_run_at(C,println(hello(X))),

 i_run_at(C,X<=100),
 i_run_at(C,println(bye(X))),
 i_run_at(C,findall(Y,member(Y,[a,b,c]),Ys)),
 i_disconnect(C),
 println(xs(Ys)),
 println(done(X)).

i_client_test(N,Len):-
  time(i_server(println([loops(N),data_length(Len)]),S)),
  i_connection(S,C),
  time(isock_test1(C,N,Len)),
  %i_disconnect(C),
  i_stop(C).

isock_test1(C,N,Len):-
  i_run_at(C,engines(ES)),println(engines(ES)),
  i_run_at(C,symbols(SS)),println(symbols(SS)),
  i_run_at(C,flags(FS)),println(FS),
  numlist(1,Len,Xs),
  for(_,1,N),
    i_run_at(C,length(Xs,_)),
    %println(Xs+L),
  fail.
isock_test1(_,N,Len):-
  println(done(N,len(Len))).  

no_client_test(N,Len):-
  time(no_client_test1(N,Len)).

no_client_test1(N,Len):-  
  numlist(1,Len,Xs),
  for(_,1,N),
    call(length(Xs,_)),
    %println(Xs+L),
  fail.
no_client_test1(N,Len):-
  println(done(N,len(Len))).  
  
  
fake_client_test(N,Len):-
  time(no_client_test1(N,Len)).

fake_client_test1(N,Len):-
  numlist(1,Len,Xs),
  for(_,1,N),
    export_term(Xs),
    import_term(Ys),
    call(length(Ys,_)),
    %println(Xs+L),
  fail.
fake_client_test1(N,Len):-
  println(done(N,len(Len))).
  
  
% shared memory - store large objects to files

shm_test:-
 N is 3/7,
 T=f(A,A,g(hello,N)),
 shm_put(boo,T),
 shm_get(boo,X),
 shm_remove(boo),
 println(T=X),
 numlist(1,1000000,L),
 time(shm_put(boo,L),T1),
 time(shm_get(boo,_),T2),
 shm_remove(boo),
 println(put(T1)+get(T2)).
   
   
   
 
% tests immediate update semantics for db
  test_db_has_immediate_update:-
    Db='test_db',
    foreach(
      between(1,10,I),
      db_assertz(Db,a(I))
    ),
    foreach(
      between(11,16,I),
      (db_retract1(Db,a(_)),
       db_assertz(Db,a(I))
      )  
    ),
    println('should go 7-16 in sequence'),
    db_listing(Db).
       

    
% various builtin tests
    
xtokenizer_test(X,Y):-xcall(57,i(X),Y).
xtokenizer_test(X):-xtokenizer_test(X,Y),write(Y),nl.

tokenizer_test:-xtokenizer_test(x).



lazy_test:-lazy_test(_,_).

lazy_test-->
  lazy_set_state(X,member(X,[a,b,c,d])),
  lazy_take(2,Xs),
  {println(Xs)},
  lazy_take(3,Ys),
  {println(Ys)},
  lazy_drop(2),
  lazy_take(2,Zs),
  {println(Zs)}.

% xf:-fconcat_test(progs).

fconcat_test(Dir):-
  files(Dir,Fs),
  %println(concatenate_files(Fs)),
  findall(DF,(member(F,Fs),concat_atom([Dir,'/',F],DF)),DFs),
  time(concatenate_files(DFs,'temp.cat')),
  ls,
  delete_file('temp.cat'),
  true.
  
  
/*
% see xdb.pl
htest:-
  assert(a(111)),
  assert(a(222)),
  assert((b(X):-a(X))),
  assert(a(111)),
  assert(a(222)),
  retract1(a(222)),
  b(X),
  println(X),
  fail.
htest.



% tests

xfile:-xfile('../psrc/top.pl').

xfile(F):-rfile('x.txt',xterm_of,F).

rfile(XF,R,F):-
  open(XF,'write',O),
  ( find_file(F,I),
    call(R,I,T),
    println(O,T),
    fail
  ; true
  ),
  close(O).
  
tm(R):-toks2term(['(',num(1),'+',num(2),'+',num(3),')','*',var('X',A),'*',var('Y',A)],R).

tt1:-tt('progs/simple.pro').
tt2:-tt('progs/pbm.pl').

tt3:-goal_tokens_of('f(X,Y,g(X),3.14).',R,Vs),
     println(R+Vs),fail.

tt4:-xterm_of(simple,T),println(T),fail;nl. 
     
bm1:-term0_of('progs/pbm.pl',_),fail.
bm1.

bm2:-xterm_of(pbm,_),fail.
bm2.

bm3:-term_of('progs/pbm.pl',_),fail.
bm3.

tt:-
  xterm_of('../../psrc/top.pl',_T),
  fail.
  
tt(F):-
  clause_tokens_of(yes,F,S,Vs,L1,L2),
    %element_of(T,S),
    println(tokens:S+vars:Vs),
    println('-------------'),
    S\=[],
    ( toks2term(S,X)->
      println(X)
    ; println('SYNTAX_ERROR BETWEEN LINES'(L1,L2))
    ),
    println('============================='),
  fail.
tt(_):-println('**********************'),nl.
*/
  
  
eb_test:-
   open_eb(E),
     eb_consult(nrev,E),call_with(E,nrev([1,2],R)),
   close_eb(E),
   println(R).

eb_bm:-
   new_eb(E),
     eb_consult(nrev,E),
     eb_listing(E),
     call_with(E,small),
   stop(E).

/*
eb_nobug:-
   new_eb(E),
     eb_consult('../tests/ebug.pl',E),
     (call_with(E,c(R)),
      println(R),
      
      fail
      ;true
     ),
   stop(E),
   symgc.

debug_engines(G):-
 engines(NE),traceln('>>> engines'(NE)),
 (G->Ok=true;Ok=fail),
 engines(EE),traceln('<<< engines'(EE)),
 println('###'(G)),cnl,
 Ok.
*/
  
% BinProlog based parser does not handle large numbers !!!
% SWI also gets it wrong for giant floats
iso_nums(88,int).
iso_nums(99.99,float).
iso_nums(11111111111111111111111111111111111111111111,bigint).
iso_nums(22222222222222222222222222222222222222222222.22222222222222222222222222222222,bigdec).
iso_nums('3333333333333333333333333333333333333333333',stringint).
iso_nums('4444444444444444444444444444444444444444444.44444444444444444444444444444444',stringdec).

iso_test:-iso_nums(X,Cmt),type_of(X,T),Z is X+0,type_of(Z,TT),write(Cmt+X:T=TT:Z),nl,fail;nl.

iso_string(X):-X=t(float(3.3)).

iso_test1:-iso_string(X),arg(1,X,Y),arg(1,Y,A),arg(1,A,B),type_of(B,T),println(T:B).


natoms:-foreach(current_non_atom(X),(tab(4),traceln(X))).

  
ebugtest:-
  new_engine(ok,fail,E),
  engine_get(E,_),
  natoms,
  symgc,
  traceln('AFTER symgc').
  
  
sproblemtest:-bg(s_server),s_wait,s_call(current_engine(X)),writeln(X).

sproblemok:-
  bg(s_server),s_wait,s_call(list_to_array([1,2,3],A)),array_to_list(A,X),writeln(X),
  s_stop.


complextest:-complextest(100).

complextest(N):-
  (s_ping->true;bg(s_server)
  ),
  s_wait,
  foreach(complextest1(N,T),run_complex_test(T)).
  
  
complextest1(N,T):-
  for(I,1,N),
  forxss(I,Xs),
  T=Xs.
  
run_complex_test(T):-bg(run_complex_test1(T)).

run_complex_test1(T):-
  current_engine(E),
  s_call(length(T,L)),
  traceln(L=E).
    

bigprinttest:-numlist(1,100000,Xs),
    println(Xs),
    fail.
    
    
  % fold test

rcons(Y,X,[X|Y]).

ereverse(Xs,Ys):-
  new_engine(X,member(X,Xs),E),
  efoldl(E,rcons,[],Ys).        

freverse(Xs,Ys):-
  foldall(rcons,X^(X=[];member(X,Xs)),Ys). 
  
  
rbug:-  
   call_java_class_method('vm.logic.Interact',rbug(a,'$null'),D),
   println(D).
   
symgctest:-
  N=4000000,
  symgctest(N).
     
symgctest(N):-
  M is N // 10,
  between(1,N,I),
    atom_concat(a,I,A),
    0 =:= I mod M,
    write(A),write(' '),
  fail
; statistics. 


bg_test:-pardb(bg).
   
clone_test:-pardb(bg_clone).
   
pardb(BgOp):-
  (retract(test_hub(Hub)),stop(Hub),fail;true),
  stats,
  
  index(a(1)),
  N=20000,
  
  for(K,0,7),
     call(BgOp,pardb(K,N),H),
     assert(test_hub(H)),
  fail.
pardb(BgOp):-traceln(thread_alunched_with(BgOp)).
  
pardb(K,N):-
  Ms is K*100,
  sleep_ms(Ms),
  thread_count(Ts),
  traceln(starting(pardb(K,N,Ts))),
  atom_concat(db,K,Db),
  foreach(
    for(I,1,N),
    (
      atom_concat(s,I,S),
      db_asserta(Db,a(S))
    )
  ),
  % symgc,
  foreach(
    for(I,1,N),
    (
      atom_concat(s,I,S),
      do_goal(Db,a(S))
    )
  ),
  foreach(
    for(I,1,N),
    (
       atom_concat(s,I,S),
       db_retract(Db,a(S))
    )
  ),
  % symgc,
  symbols(Syms),
  %db_size(DS),
   thread_count(TsAgain),
  traceln(done(K,N,Syms,TsAgain)).

/*  
% compiler_bug: traceln executed twice, only if compile with lco !!!  
% FIXED in showSingleVars
compiler_bug:- 
  Fname=a,
  write(hereTwice),nl,
  fail.
 
*/  

  