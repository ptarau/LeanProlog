% meta.pl

/* based on R.A. O'Keefe's meta-circular interpreter (see also extra.pl) 

  depends on: 
    default_eb, eb_is_dynamic, eb_clause, 
    do_goal, db_is_dynamic, db_clause
*/




% do_goal(X):-do_xdyngoal('$',X).

% changed when we want edb.pl on-heap db
do_goal(X):-eb_active(Db),!,do_goal(Db,X).
do_goal(X):-do_goal('$',X).



db_call(Db,Goal):-do_goal(Db,Goal).

% metacall should just call this - clearly it is about a term-goal
do_goal(_Db,Goal) :-
  is_compiled(Goal),
  !,
  Goal.
do_goal(Db,Goal):-
  atom(Db),
  !,
  do_xdyngoal(Db,Goal).
do_goal(Db,Goal):-
  is_engine(Db),
  !,
  do_edyngoal(Db,Goal).
do_goal(Db,G):-
  errmes(bad_database(Db),when_calling(G)).
  
do_edyngoal(Db,Goal) :- % in fact, this can be safely left to the interpreter once it handles ! and calls back the compiler
  eb_is_dynamic(Db,Goal),
  !,
  eb_clause(Db,Goal, Body),	% <--- assume everything else is interpreted
  do_body(Body, AfterCut, HadCut,Db),
  (   HadCut = yes,
	  !,
	  call_with(Db,AfterCut)
	; HadCut = no
   ).
do_edyngoal(Db,Undef):-
  procedure_existence_error(Db,Undef).

do_xdyngoal(Db,Goal) :- % in fact, this can be safely left to the interpreter once it handles ! and calls back the compiler
  db_is_dynamic(Db,Goal),
  !,
  db_clause(Db,Goal, Body),	% <--- assume everything else is interpreted
  do_body(Body, AfterCut, HadCut,Db),
  (   HadCut = yes,
	  !,
	  call_with(Db,AfterCut)
	; HadCut = no
   ).  
do_xdyngoal(MyDb,Goal) :- % inherits from shared -- see @
  get_supers(MyDb,Supers),
  append(Supers,['$'],SupersAndDefault),
  member(Db,SupersAndDefault), % Try to inherit from supers and shared Db='$'
  db_is_dynamic(Db,Goal),
  !,
  db_clause(Db,Goal, Body),	% <--- assume everything else is in shared Db
  do_body(Body, AfterCut, HadCut,Db),
  (   HadCut = yes,
	  !,
	  call_with(Db,AfterCut)
	; HadCut = no
   ).      
do_xdyngoal(Db,Undef):-
   procedure_existence_error(Db,Undef).

procedure_existence_error(Db,Undef):-
  current_engine(E),
  IMPLEMENTATION_DEPENDENT_TERM=[db(Db),E,bad_call(Undef)],
  build_undef_err(Undef,Mes),
  errmes(Mes, IMPLEMENTATION_DEPENDENT_TERM).
  
build_undef_err(Undef,Mes):-var(Undef),!,
  Mes=instantiation_error.
build_undef_err(Undef,Mes):-atomic(Undef),!,
  Mes=existence_error(procedure,Undef/0).
build_undef_err(Undef,Mes):-arg(0,Undef,F),arity(Undef,N),
  Mes=existence_error(procedure,F/N).
  
%% get_supers(X,Supers): gets the list of agents that X follows in this space
get_supers(MyDb,Supers):-MyDb==>follows(Xs),nonvar(Xs),!,Supers=Xs.
get_supers(_,[]).
  

%% call_with(Db,Body): metainterpreter, evaluates Body using clauses from database Db  
call_with(Db,Body):-var(Body),!,procedure_existence_error(Db,Body).
call_with(Db,Body) :-
	do_body(Body, AfterCut, HadCut,Db),
	( HadCut = yes,
		!,
		call_with(Db,AfterCut)
	;   HadCut = no
	).

do_body((Goal,Body), AfterCut, HadCut,Db):-!,
  do_conj(Goal,Body, AfterCut, HadCut,Db).
do_body((Goal;Body), AfterCut, HadCut,Db):-!,
  do_disj(Goal,Body, AfterCut, HadCut,Db).
do_body(!, true, yes,_):-!.
do_body(Goal, true, no,Db) :- 
  do_goal(Db,Goal).

do_conj(V,AfterCut, AfterCut, yes,Db) :- var(V),!,
  procedure_existence_error(Db,V).
do_conj(!,AfterCut, AfterCut, yes,_) :- !.
do_conj(Goal,Body, AfterCut, HadCut,Db) :- 
  call_with(Db,Goal),
  do_body(Body, AfterCut, HadCut,Db).

do_disj((If->Then),Else, AfterCut, HadCut, Db) :-!,
  do_if_then_else(If,Then,Else, AfterCut, HadCut, Db).
do_disj(Disj1,Disj2, AfterCut, HadCut, Db) :-
  do_disj0(Disj1,Disj2, AfterCut, HadCut,Db).

do_if_then_else(If,Then,_, AfterCut, HadCut, Db) :- call_with(Db,If),!,
  do_body(Then, AfterCut, HadCut, Db).
do_if_then_else(_,_,Else, AfterCut, HadCut ,Db) :-
  do_body(Else, AfterCut, HadCut, Db).

do_disj0(Disj1,_, AfterCut, HadCut, Db) :-
  do_body(Disj1, AfterCut, HadCut, Db).
do_disj0(_,Disj2, AfterCut, HadCut, Db) :-
  do_body(Disj2, AfterCut, HadCut, Db).

   
  
%% ?G: debugging made simple: atoms annotated with '?' will have their evaluation traced

'?'(G):-
  traceln(calling_(G)),
  if_any(G,trace_success(G),trace_failure(G)).

trace_success(G):-traceln(exiting_(G)).

trace_failure(G):-traceln(failing_(G)),fail. 
	
