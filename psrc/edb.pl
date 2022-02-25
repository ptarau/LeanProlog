% edb.pl

/*
  Abstract dynamic database API using engines as server agents
*/

eb_small_test:-
  new_eb(D),eb_consult(nrev,D),call_with(D,small). 

open_eb:-open_eb(_).

open_eb(Db):-new_eb(Db),set_val('$db',Db),protect_engine(Db).

close_eb:-default_eb(Db),close_eb(Db).
close_eb(Db):-remove_val('$db'),unprotect_engine(Db),stop(Db).

default_eb(Db):-get_val('$db',V),!,V=Db.
default_eb(Db):-open_eb(Db).             

eb_active(Db):-get_val('$db',Db).

% inactive

eb_call(T):-default_eb(Db),call_with(Db,T).
 
eb_assertz(X):-default_eb(Db),eb_assertz(Db,X).

eb_asserta(X):-default_eb(Db),eb_asserta(Db,X).

eb_assert(X):-eb_assertz(X).

eb_clause(H,B):-default_eb(Db),eb_clause(Db,H,B).

eb_retract1(H):-default_eb(Db),eb_retract1(Db,H).

eb_retract(H):-default_eb(Db),eb_retract(Db,H).

eb_retractall(H):-default_eb(Db),eb_retractall(Db,H).

eb_abolish(F,N):-default_eb(Db),eb_abolish(Db,F,N).

eb_is_dynamic(H):-default_eb(Db),eb_is_dynamic(Db,H).

eb_dynamic(FN):-default_eb(Db),eb_dynamic(Db,FN).

elisting:-default_eb(Db),eb_listing(Db).
elisting(X):-default_eb(Db),eb_listing(Db,X).


eb_reconsult(InFile):-default_eb(Db),eb_reconsult(InFile,Db).

eb_consult(InFile):-default_eb(Db),eb_consult(InFile,Db).


eb_reconsult(InFile,Db):-
  eb_clear(Db),
  println(clearing_database),
  eb_consult(InFile,Db).
  
eb_consult(InFile0,Db):- 
  find_file(InFile0,InFile),
  println(
     all_clauses(from(InFile),will_be_asserted) 
  ),
  ( term_of(InFile,T0),
    expand_term(T0,T),
    eb_assertz(Db,T),
    fail
  ; true  
  ).


eb_clear:-default_eb(Db),eb_clear(Db).

eb_listing:-default_eb(E),eb_listing(E).

eb_listing(Db):-(eb_clause(Db,H,B),portray_clause((H:-B)),fail;nl).

eb_listing(Db,F/N):-!,functor(H,F,N),(eb_clause(Db,H,B),portray_clause((H:-B)),fail;true).
eb_listing(Db,F):-
  edb_max_arity(Arity),
  (between(0,Arity,K),eb_listing(Db,F/K),fail;nl).

% creates a new engine server providing Prolog database operations
new_eb(Engine):-new_engine(done,hqueue_server,Engine).
% adds an element to the end of the database
eb_assertz(Engine,C):-to_clause(C,Clause),ask_engine(Engine,add_element(Clause),the(yes)).
% adds an element to the front of the database
eb_asserta(Engine,C):-to_clause(C,Clause),ask_engine(Engine,push_element(Clause),the(yes)).
% returns a instances of asserted clauses
eb_is_dynamic(Engine,Head0):-
  functor(Head0,F,N),functor(Head,F,N),
  Clause=(Head:-_Body),
  ask_engine(Engine,is_dynamic(Clause),the(yes)).  
eb_clause(Engine,Head,Body):-
  eb_clauses(Engine,Head,Xs),
  member((Head:-Body),Xs).
% delete an element of the database
eb_retract1(Engine,Head):-
  Clause=(Head:-_Body),
  ask_engine(Engine,delete_element(Clause),the(yes(Clause))).
eb_retractall(Engine,Head):-
  Clause=(Head:-_Body),
  ask_engine(Engine,delete_all(Clause),the(yes)).  
% clears a database  
eb_clear(Engine):-
  ask_engine(Engine,clear,the(yes)).
    
% removes a database
eb_delete(Engine):-stop(Engine).

eb_retract(Db,H):-eb_retract1(Db,H),eb_retract_more(Db,H).

eb_retract_more(_,_).
eb_retract_more(Db,H):-eb_retract(Db,H).

% eb_retractall(Db,H):-repeat, not(eb_retract1(Db,H)),!.

eb_abolish(Engine,F,N):-functor(H,F,N), eb_retractall(Engine,H). 

eb_clauses(Engine,Head,Cs):-
  ask_engine(Engine,queue(Head),the(Cs)).
  
eb_dynamic(Db,F/N):-
 functor(T,F,N),eb_asserta(Db,T),eb_retract1(Db,T).

get_slot_key(add_element((H:-_)),H).
get_slot_key(push_element((H:-_)),H).
get_slot_key(queue(H),K):-nonvar(H),K=H.
get_slot_key(delete_element((H:-_)),H).
get_slot_key(delete_all((H:-_)),H).
get_slot_key(is_dynamic((H:-_)),H).

/*
  Helper predicates/Implementation
*/

edb_max_hash(4096).
edb_max_arity(256).

hqueue_server:-edb_max_hash(Size),hserver(Size).

hserver(N):-
  hinit_eb(N,D),
  hserver(D,N).
    
hserver(D,N):-
  from_engine(Q),
  h_server_step(Q,D,NewD,N, A),
  return(A),
  hserver(NewD,N).

hinit_eb(N,D):-functor(D,'$',N).


%hinit_slot(S,Q):-println(here=Q+S),fail. % \=(Q,eb_is_dynamic(_)),
hinit_slot(S,_):-var(S),!,S=(Xs-Xs).
hinit_slot(_,_).

same_vars1(0,1):-!,fail.
same_vars1(1,0):-!,fail.
same_vars1(_,_).

same_vars(X,Y):-var(X),var(Y),same_vars1(X,Y).

h_standard_step(is_dynamic(_),_I,_D,S, A):- nonvar(S),S=Xs-Ys,same_vars(Xs,Ys),!,A=no.
h_standard_step(Q,I,D,S, A):-
  hinit_slot(S,Q),
  S=Hs1-Ts1,
  server_task(Q, Hs1,Ts1, Hs2,Ts2, A),
  NewS=Hs2-Ts2,
  change_arg(I,D,NewS).


h_server_step(Q,D,D,N, A):-
  get_slot_key(Q,K),
  hkey(N,K,I0),succ(I0,I),
  arg(I,D,S),
   
  h_standard_step(Q,I,D,S, A),
  !.
h_server_step(queue(X),D,D,N, Cs):-var(X),
  collect_matching_clauses(D,N,Cs),
  % alt: collect_matching_clauses1(D,N,Cs),
  !.
h_server_step(clear,_,D,N, yes):-
  hinit_eb(N,D),
  !.  
h_server_step(Q,D,D,_N,failed(Q)).
   
collect_matching_clauses(D,N,Cs):-
  collect_matching_cs(0,N,D,_,Css),
  append(Css,Cs).
  
collect_matching_cs(N,N,_,_,[]):-!.
collect_matching_cs(K,N,D,C0,[Ms|Mss]):-
  succ(K,K1),
  arg(K1,D,CsEs),
  nonvar(CsEs),
  CsEs=Cs-_,
  nonvar(Cs),
  !,
  collect_matches(Cs,C0,Ms),
  collect_matching_cs(K1,N,D,C0,Mss).
collect_matching_cs(K,N,D,C0,Css):-
  succ(K,K1),
  collect_matching_cs(K1,N,D,C0,Css).
 
collect_matches(Cs,_,Ms):-var(Cs),!,Ms=[].
collect_matches([C|Cs],C0,Ms):- \=(C,C0),!, collect_matches(Cs,C0,Ms).
collect_matches([C|Cs],C0,[C|Ms]):- collect_matches(Cs,C0,Ms).

server_task(add_element(X), Xs,[X|Ys], Xs,Ys, yes).
server_task(push_element(X),Xs,Ys,[X|Xs],Ys,yes).
server_task(queue(H),Xs,Ys,Xs,Ys, Rs):-collect_matches(Xs,(H:-_),Rs).
                                % alt: collect_matches1(Xs,Ys,(H:-_),Rs).
server_task(delete_element(X),Xs,Ys,NewXs,Ys,YesNo):-
   server_task_delete(X,Xs,NewXs,YesNo).
server_task(delete_all(X),Xs,Ys,NewXs,Ys,YesNo):-
   server_task_delete_all(X,Xs,NewXs,YesNo).
server_task(is_dynamic(X),Xs,Ys,Xs,Ys,YesNo):-
   server_task_is_dynamic(X,Xs,Ys,YesNo).
server_task(invoke_server_task(Goal),Xs,Ys,Xs,Ys,Goal):-
   if(Goal,true,true).

server_task_delete_all(X0,Xs,NewXs,yes):- delete_matches(Xs,X0,NewXs).

delete_matches(Xs,_,Ys):- var(Xs),!,Ys=Xs.
delete_matches([X|Xs],X0,[X|Ys]):- \=(X,X0), !,delete_matches(Xs,X0,Ys).
delete_matches([_X|Xs],X0,Ys):- delete_matches(Xs,X0,Ys).

server_task_delete(X,Xs,NewXs,YesNo):-
  select_nonvar(X,Xs,NewXs),
  !,
  YesNo=yes(X).
server_task_delete(_,Xs,Xs,no).

%server_task_is_dynamic(X,Xs,Ys,no):-traceln(is_dynamic_called(X,Xs,Ys)),fail.
server_task_is_dynamic(X,Xs,Ys,no):- not(pick_clause_from(Xs-Ys,X)),!.
server_task_is_dynamic(_X,_Xs,_Ys,yes).

pick_clause_from(CsEnd,C):-
  nonvar(CsEnd),
  CsEnd=Cs-[],  
  member(C,Cs).
  
/*

% alt
collect_matching_clauses1(D,N,Cs):-findall(C,pick_a_clause(D,N,C),Cs).

collect_matches1(Cs,Es,C0,Ms):-findall(C0,pick_clause_from(Cs-Es,C0),Ms).

pick_a_clause(D,N,C):-
  between(1,N,I),
  arg(I,D,CsEnd),
  pick_clause_from(CsEnd,C).
   
*/



