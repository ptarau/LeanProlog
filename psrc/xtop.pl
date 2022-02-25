% xtop.pl

%% is_prolog(Prolog): tells which Prolog is this: returns lprolog
is_prolog(lprolog).

version_data([5,0,0,[]]). %b write_canonical

prolog_init(yes):-
  version_data([A,B,C|_]),
  cwrite('Lean Prolog (64 bits) '),
  cwrite(A),cwrite('.'),cwrite(B),cwrite('.'),cwrite(C),cwrite(' '),
  cwrite('Copyright Paul Tarau 2022'),cnl,
  cwrite('Licence: APACHE 2.0'),cnl,
  %traceln('License: free for research and academic use.'),
  call_ifdef(guardian_archangel,call_ifdef(guardian_angel,true)).


%% apply_do_after_load is called before toploop to run dynamic events shcheduled by the compiler
apply_do_after_load:-
  ( foreach(member(G,[comps_version(_),nlp_version(_),vivo_version(_)]),show_logo(G)),fail
  % ; is_compiled('$$_do_after_load(_)),foreach(do_after_load(G),topcall(G)),fail
  ; 'apply_$$_do_after_load',fail
  ; true
  ).

%'apply_$$_do_after_load':-'$$_init_done'==>done,!. 
'apply_$$_do_after_load':-
%  '$$_init_done'<==done, 
  get_verbosity(V),
  Max is 1<<16,
  for(I,0,Max),
    atom_concat('$$_do_after_load_',I,Name),
    functor(Pred,Name,1),arg(1,Pred,G),
    ( \+ is_compiled(Pred) -> !, 
      (V>2->traceln(total_scheduled_initializations(I));true)
    ; 
      call(Pred),
      (V>3->traceln(Pred);true),
      topcall(G),
      fail
    ).
   
show_logo(Pred):-arg(1,Pred,V),is_compiled(Pred),Pred,traceln(V).

 
%% entry point in Prolog interactive loop     
toploop:-
  %traceln(starting(toploop)),
  toploop(main).

%% entry point in a module, in particular 'main'
toploop(Module):-
  traceln(entering(Module)),
  apply_do_after_load,
  run_cmd_args,
  crossref_warnings,
  toplevel(Module),
  traceln(exiting(Module)).

run_cmd_args:-
  % traceln(run_cmd_args),
  pop_cmd_arg(T),
  %traceln(popping_cmd_argument(T)),
  nonvar(T),
  write_codes("?- "),portray_clause(T),
  run_cmd_arg(T),
  !,
  run_cmd_args.
run_cmd_args.

run_cmd_arg(Var):-var(Var),!,traceln(bad_cmd_arg_should_be_nonvar).
run_cmd_arg([File]):-!,compile(File).  
run_cmd_arg(Goal):-
  topcall(Goal),
  fail.
run_cmd_arg(_).
 
% interactive toplevel
 


/*  
open_top(Db,State):-

  open_eb(Db),
  open_state(State).
  
close_top(Db,State):-
  close_eb(Db),
  close_state(State),

  true.
*/
    
toplevel(Module):-
  stdio(I),
  trim_module_name(Module,Prompt),
  X=ignore,
  G=toplevel1(Prompt,I),
  new_engine(X,true,E),
  
  repeat,
    load_engine(E,X,G),
    get(E,A),
  handle_top_returns(A).
 

handle_top_returns(the(done)):-!. % exit
handle_top_returns(the(restart(F0))):-!,traceln(restarting_from(F0)).
handle_top_returns(the(exception(E))):-!,write('*** '),println(E),fail.

trim_module_name(main,main):-!.
trim_module_name(Module,Prompt):-atom_codes(Module,Cs),
  reverse(Cs,Rs),
  member(S,"/\\"),
  append(Xs,[S|_],Rs),
  !,
  reverse(Xs,Ps),
  atom_codes(Prompt,Ps).
trim_module_name(M,M).
  
toplevel1(Prompt,I):-
  repeat,
    engine_gc,
    topstep1(Prompt,I), % exit only with exceptions
  fail.
 
topstep1(Prompt,I):-
  interact_with(Prompt,I, T,Vs),
  !,
  reverse(Vs,RVs),
  topstep2(topcall,T,RVs,_CharRead).
topstep1(_Prompt,_I):-    
  % writeln('toplevel_syntax_error.'),
  true.

interact_with(Prompt,I, T,Vs):-
  write(I,Prompt),put_code(I,32),
  readln_goal(I,T,Vs).
  %nonvar(T).


topstep2(F,T,[],10):-!,topstep_novars(F,T).
topstep2(F,T,[X-V|XVs],C):-
  (
    call(F,T),
    numbervars(T,0,_), 
    show_var(X,V),
    ( member(Xk-Vk,XVs),
      write(', '),
      show_var(Xk,Vk),
      fail
    ; kbd_wait(C)-> % fails if not stdio
      ([C]=";"->
       writeln(' ;')
       ; !, nl,topstep_end
      )
    ; writeln(' ;')
    ),
    fail
  ; topstep_end
  ).
  
topstep_end:-nl,writeln('No (more) answers.'),nl.

kbd_wait(C):-current_input(I),stdio(I),get_code(I,C).

topstep_novars(F,T):-call(F,T),!,writeln('true.').
topstep_novars(_,_):-writeln('fail.').
  
show_var(X,A):-[SP,EQ]=" =",write(X),put_code(SP),put_code(EQ),put_code(SP),write(A).


% end
