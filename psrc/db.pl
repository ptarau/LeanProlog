% db.pl

% BASIC INDEXED STORE API

xdb_push(Db,Key0,Val,Ref):-
  xdb_put(0,Db,Key0,Val,Ref).

xdb_put(Db,Key0,Val,Ref):-
  xdb_put(1,Db,Key0,Val,Ref).

xdb_put(AtEnd,Db,Key0,Val,Ref):-
  xdb_apply_index(Db,Key0,Key),
  to_bundle(Val,Ref),
  jcall('vm.extensions.DynamicDB',put(Key,Ref,AtEnd),Ok),
  Ok=1.

/*  
xdb_get(Db,Key0,Val,Ref):-
  xdb_apply_index(Db,Key0,Key),
  jcall('vm.extensions.DynamicDB',get(Key),Bs),
  member(Ref,Bs),
  from_bundle(Ref,Val).
*/

xdb_get(Db,Key0,Val,Ref):-
  xdb_apply_index(Db,Key0,Key),
  jcall('vm.extensions.DynamicDB',get(Key),Iterator),
  ielement_of(the(Iterator),Ref),
  from_bundle(Ref,Val).

xdb_remove(Db,Key0,Ref,R):-
  xdb_apply_index(Db,Key0,Key),
  jcall('vm.extensions.DynamicDB',remove(Key,Ref),R).

xdb_size(R):-jcall('vm.extensions.DynamicDB',size,R).
  
xdb_clear:-
  jcall('vm.extensions.DynamicDB',clear,_).

xdb_indexer(I):-jcall('vm.extensions.DynamicDB',getIndexer,I).

xdb_show:-jcall('vm.extensions.DynamicDB',showAll,_).

xdb_index(Spec):-
  traceln(indexing(Spec)),
  jcall('vm.extensions.DynamicDB',index(Spec),_).

/*
xdb_apply_index(Db,H,K):-var(H),
  warnmes(unbound_args_in,xdb_apply_index(Db,H)),
  fail.
xdb_apply_index(Db,H,K):-var(Db),
  warnmes(unbound_args_in,xdb_apply_index(Db,H)),
  fail. 
*/ 
xdb_apply_index(Db,H,K):-
  atom(Db),
  !,
  trim_funs(H,H0),
  % H=H0,
  jcall('vm.extensions.DynamicDB',applyIndex(Db,H0),K).
xdb_apply_index(Db,H,_K):-
  errmes(should_be_an_atom,database_name(Db,H)).
  
trim_funs(FXs,FTs):-shallow_clone(FXs,FTs,N),trim_shallow(N,FXs,FTs).

trim_shallow(0,_,_):-!.
trim_shallow(I,FXs,FTs):-I>0,I1 is I-1,
  arg(I,FXs,X),arg(I,FTs,T),
  fun_skeleton(X,T),
  trim_shallow(I1,FXs,FTs).

fun_skeleton(FXs,FYs):- shallow_clone(FXs,FYs,_).

shallow_clone(FXs,FTs,N):-compound(FXs),!,
   arg(0,FXs,F),arity(FXs,N),fun(F,N,FTs).
shallow_clone(X,X,0).

is_indexed(H):-atomic(H),!,fail.
is_indexed(H):-
  trim_funs(H,H0),
  % traceln(is_indexed(H0)),
  % fun_skeleton(H,H0),
  jcall('vm.extensions.DynamicDB',is_indexed(H0),1).

%% xsave(File): saves clause in default Db to compressed file
xsave(File):-xsave(File,'$').

%% xload(File): replaces clauses in default Db with clauses in compressed file
xload(File):-xload(File,'$').

%% xappend(File,Db): adds to default Db clauses in compressed file 
xappend(File):-xappend(File,'$').

%% xsave(File,Db): saves clause in database Db to one compressed file
xsave(File,Db):-
  jcall('vm.extensions.DynamicDB',xsave(File,Db),_).

%% xclause_of(File,Clause): backtracks/iterates over clauses in compressed file
xclause_of(File,Clause):-
  jcall('vm.extensions.DynamicDB',xload(File),Iterator),
  ielement_of(the(Iterator),Ref),
  from_bundle(Ref,Clause).

%% xload(File,Db): replaces all clauses in Db with clauses in compressed file - make sure indexing is on if needed!
xload(File,Db):-
  exists_file(File),
  db_clear(Db),
  xappend(File,Db).

%% xappend(File,Db): adds to Db clauses in compressed file  
xappend(File,Db):-
 xclause_of(File,Clause),
 db_assert(Db,Clause),
 fail.
xappend(_,_).   
  
%% xdb_save(File): saves all your databases to one compressed file, indexes included            
xdb_save(F):-jcall('vm.extensions.DynamicDB',toFile(F),_).

%% xdb_load(File): replaces all your databases in memory with the content, indexes included
xdb_load(F):-jcall('vm.extensions.DynamicDB',fromFile(F),_).
  
% xdb store tools

iget(I,R):-jcall('vm.extensions.DynamicDB',next(I),R).
  
ielement_of(I,X):-iget(I,the(A)),iselect_from(I,A,X).

iselect_from(_,A,A).
iselect_from(I,_,X):-ielement_of(I,X).

db_set_dynamic(Db,H):- fun_skeleton(H,H0),jcall('vm.extensions.DynamicDB',setDynamic(Db,H0),_).
db_remove_dynamic(Db,H):-fun_skeleton(H,H0),jcall('vm.extensions.DynamicDB',removeDynamic(Db,H0),_).

% PROLOG DYNAMIC DATABASE


%% db_dynamic, db_is_dynamic, db_abolish : same as their counterparts wihtouh 'db_', except that extra first argument names a database on which they act
%% db_assert, db_retract etc: same as assert, retract but extra first argument names a database on which they act

db_dynamic(Db,F/N):-!,fun(F,N,H),db_set_dynamic(Db,H).
db_dynamic(Db,(FN,FNs)):-db_dynamic(Db,FN),db_dynamic(Db,FNs).

% db_is_dynamic(Db,H):-traceln(db_is_dynamic(Db,H)),fail.
db_is_dynamic(Db,H):-nonvar(Db),symbolic(H),!,
  fun_skeleton(H,H0),
  jcall('vm.extensions.DynamicDB',isDynamic(Db,H0),1).
db_is_dynamic(Db,H):-procedure_existence_error(Db,H).

db_current_predicate(Db,F/N):-
  jcall('vm.extensions.DynamicDB',allDynamics,TripleIterator),
  ielement_of(the(TripleIterator),'$'(Db,F,N)).
 
db_abolish(Db,F/N):-(var(Db);var(F);var(N)),!,
  db_current_predicate(Db,F/N),
  db_abolish0(Db,F/N).
db_abolish(Db,(F/N,More)):-!,
  db_abolish0(Db,F/N),
  db_abolish(Db,More).
db_abolish(Db,FN):-
  db_abolish0(Db,FN).

db_abolish0(Db,F/N):-
  fun(F,N,H),
  db_retractall(Db,H),
  db_remove_dynamic(Db,H).

db_abolish_dynamic(Db,F/N):-
  fun(F,N,H),
  db_retractall(Db,H).

db_assert(Db,C):-
  to_clause(C,(H:-B)),
  db_set_dynamic(Db,H),
  xdb_put(Db,H,(H:-B),_),
  !.
db_assert(Db,C):-
  assert_errmes(C,Mes),
  errmes(Mes,db_assert(Db,C)).

db_assertz(Db,C):-db_assert(Db,C).

db_asserta(Db,C):-
  to_clause(C,(H:-B)),
  db_set_dynamic(Db,H),
  xdb_push(Db,H,(H:-B),_),
  !.
db_asserta(Db,C):-
  assert_errmes(C,Mes),
  errmes(Mes,db_asserta(Db,C)).

assert_errmes((H:-_),Mes):-is_indexed(H),!,Mes=failed_to_assert(indexed_rule).
assert_errmes(H,Mes):-is_indexed(H),!,Mes=failed_to_assert(indexed_fact).
assert_errmes(_,failed_to_assert).

db_clause(Db,H,B):-xdb_get(Db,H,(H:-B),_).

db_clause(Db,H,B,Ref):-xdb_get(Db,H,(H:-B),Ref).

db_erase(Db,H,Ref):-xdb_remove(Db,H,Ref,_).

db_retract(Db,C):-
  to_clause(C,(H:-B)),
  xdb_get(Db,H,(H:-B),Ref),
  xdb_remove(Db,H,Ref,_).

db_retract1(Db,H):-db_retract(Db,H),!.
  
db_retractall(Db,H):-
    db_retract(Db,(H:-_)),
  fail.
db_retractall(_,_).

%% db_clear(Db) : clears all caluses of database Db 
db_clear(Db):-db_abolish(Db,_),fail.
db_clear(_).


db_listings(Stream):-db_listing(Stream,_Db).

db_listing(Db):-current_output(S),db_listing(S,Db).

db_listing(Stream,Db):-db_listing(Stream,Db,_).

db_listing(Stream,Db,FN0):-
  (atom(FN0)->FN=FN0/_;FN=FN0),
  foreach(
    db_current_predicate(Db,FN),
    db_listing1(Stream,Db,FN)
  ).
  
db_listing1(S,Db,F/N):-
   nl(S),fun(F,N,H),
   write(S,'% '),write(S,Db),write(': '),write(S,F/N),write(S,'.'),nl(S),
     db_clause(Db,H,B),
     portray_clause(S,(H:-B)),
   fail.
db_listing1(S,_,_):-nl(S).

%% db_reconsult(InFile,Db) reconsults InFile to database named Db
db_reconsult(InFile,Db):-
  db_clear(Db),
  %println(clearing_database),
  db_consult(InFile,Db).

%% db_consult(InFile,Db) consults InFile to database named Db  
db_consult(InFile,Db):- 
  %db_show_progress(InFile),
  ( term_of(InFile,T0),
    expand_term(T0,T),
    db_assert(Db,T),
    fail
  ; true  
  ). 

db_show_progress('$str'(_)):-!,println(consulting_from_string).
db_show_progress(_).
  
% DEFAULT DB is simply '$'

%% topcall(Goal) : calls Goal trying to use clauses from default database
topcall(T):-eb_active(Db),!,call_with(Db,T).
topcall(T):-call_with('$',T).

%%  db_ensure_bound(Db) - generates, if argument Db is unbound, '$0', '$1', .. as Db name
db_ensure_bound(Db):-nonvar(Db),!.
db_ensure_bound(Db):-global_gensym('$',Db).

%% index(Spec): indexes arg positions marked 1 in Spec ; index implies dynamic as it only indexes dynamics
index(Spec):-arg(0,Spec,F),arity(Spec,N),xdb_index(Spec),dynamic(F/N).

%% is_dynamic(H): checks if a predicate H=F(X1..Xn) is dynamic
is_dynamic(H):-db_is_dynamic('$',H).

%% dynamic(F/N): ensures predicate F/N will is dynamic - it is ok to call such predicates even if none has been asserted
dynamic(FN):-db_dynamic('$',FN).

%% abolish(F/N), abolish(F,N): discards predicate F/N, calling it later triggers an error
abolish(FN):-db_abolish('$',FN).
abolish(F,N):-abolish(F/N).

%% abolish_dynamic(F/N): discards predicate F/N, such that calling it later just fails
abolish_dynamic(FN):-db_abolish_dynamic('$',FN).

%% assert(C): adds clause C to the database
assert(C):-db_assert('$',C).

%% assertz(C): adds clause C to end of the database
assertz(C):-assert(C).

%% asserta(C): adds clause C to the front of the database
asserta(C):-db_asserta('$',C).

%% clause(H,B): true if clause C=(H:-B) is in the database
clause(H,B):-db_clause('$',H,B).

%% retract(H): retracts clauses with head unifying with H from the database, one at a time
retract(H):-db_retract('$',H).
 
%% retract1(H): retracts (at most) one clause with head unifying with H from the database
retract1(H):-db_retract1('$',H).
 
%% retractall(H): retracts all clauses with head unifying with H from the database
retractall(H):-db_retractall('$',H).

%% db_clear: removes all clauses from database
db_clear:-db_clear('$').

%% listings: list clauses from all databases
listings:-
  current_output(S),
  db_listing(S,_),
  list_gvars(S).
  
listing(FN):-listing('$',FN).

%% listing(Db,F/N): lists predicate F/N in database Db
listing(Db,FN):-current_output(S),db_listing(S,Db,FN).

%% listing: list all clauses from default database
listing:-db_listing('$').

%% consult(F): asserts all clauses if F to defaul database
consult(F0):-db_consult(F0,'$').

%% reconsult(F):cleans defautl database and then asserts all clauses if F to defaul database
reconsult(InFile):-db_reconsult(InFile,'$').

%% save(Name): saves a snapshot of all databases to file Name.px
save(Name):-atom_concat(Name,'.px',XName),xdb_save(XName).

%% load(Name): loads the set of datbases from a saved snapshot Name.px
load(Name):-atom_concat(Name,'.px',XName),xdb_load(XName).

db_save(F):-db_save('$',F).

%% db_save(Db,F): saves database Db to plain text file F (with no guaranty that it can be read back!)
db_save(Db,F):-
 current_output(S0),
 open(F,write,S),
 set_output(S),
 db_listing(S,Db),
 close(S),
 set_output(S0).

%% db_save_canonical(Db,F): saves database Db to plain text file F (with guaranty that it can be read back!)
db_save_canonical(Db,F):-
  open(F,write,S),
  canonical_listing_to(S,Db),
  close(S).

canonical_listing_to(Stream,Db):-
  db_clause(Db,H,B),
  (B==true->C=H;C=(H:-B)),
  pp_canonical(Stream,C),
  fail.
canonical_listing_to(_,_).
  
pp_canonical(Stream,T):-
  write_canonical(Stream,T),
  fwriteln(Stream,'.').  

pp_canonical(T):-
 current_output(S),
 pp_canonical(S,T).

/*
junkin:-
  index(boo(1)),
  index(boo(1,1)),
  index(boo(1,1,1)),
  index(boo(1,1,1,1)),
  index(bee(1)),
  index(bee(1,1)),
  index(bee(1,1,1)),
  index(bee(1,1,1,1)),
  index(bboo(1)),
  index(bboo(1,1)),
  index(bboo(1,1,1)),
  index(bboo(1,1,1,1)),
  index(bbee(1)),
  index(bbee(1,1)),
  index(bbee(1,1,1)),
  index(bbee(1,1,1,1)).
*/

