% xlib.pl

% impure operations


%% numlist(Min,Max,Ns): generates list of ints between Min and Max
numlist(I,I,Is):-!,Is=[I].
numlist(I0,I,[I0|Is]):-I0<I,I1 is I0+1,numlist(I1,I,Is).

'..'(A,B,Is):-numlist(A,B,Is).

ints(A,B,Is):-numlist(A,B,Is).

% moved to xbuiltin.pl: atomic_list_concat(As,A):-maplist(name,As,Css)...

%% concat_atom(As,A): concatenates atoms on list As to A 
concat_atom(As,A):-atomic_list_concat(As,A).


%% digest_prolog_files(PrologFiles,OldRootWamFile,NewRootWamFile): extends bootable root wamfile with a list of prolog files
digest_prolog_files(PrologFiles,OldRootWamFile,NewRootWamFile):-
   TempWamFile='temp__.bp',
   wcompile_mt(PrologFiles,TempWamFile),
   concatenate_files([OldRootWamFile,TempWamFile],NewRootWamFile).
   
%% external_engine(WamSource,X,G,E) : creates external engine E with separate code space and goal X,G
external_engine(WamSource,X,G,E):-
  external_engine(WamSource,E),
  load_engine(E,X,G).

%% external_engine(Source,E): creates new code space + engine from a *.bp file or reader
external_engine(Source,E):-
  new_interactor('vm.logic.LogicInteractor',Source,E).
 
external_engine(E):-external_engine('lwam.bp',E).

external_top(InFile,Source):-
  %traceln(external_TOP_calling(toploop(InFile))),
  external_engine(Source,ok,toploop(InFile),E),
  get(E,_A),
  stop(E),
  %traceln(external_TOP_finished(toploop(InFile))),
  % exit,
  true.
  

init_toploop(_Fname,InitGoal):-
  %println(ee=there),
  InitGoal,
  fail.
init_toploop(Fname,_InitGoal):-
  toploop(Fname).
  

% some flags

%% set_quickfail(V,OldV): sets flag forcing quick failure (if higher) 
set_quickfail(V,OldV):-jcall('vm.logic.Interact',set_quickfail(V),OldV).

get_quickfail(OldV):-jcall('vm.logic.Interact',get_quickfail,OldV).

%% set_verbosity(V,OldV): sets flag controlling verbosity (higher) of error messages
set_verbosity(V,OldV):-jcall('vm.logic.Interact',set_verbosity(V),OldV).

get_verbosity(OldV):-jcall('vm.logic.Interact',get_verbosity,OldV).

% compatibility with other Prologs

%% for(I,A,B): generates ints I in [A..B]
for(I,A,B):-between(A,B,I).

ctime(T):-cputime(T0),T is T0*1000.

%% statistics: prints out stats about various data areas
statistics:-(statistics(A,B),writeln(A=>B),fail;nl).

stats:-statistics.

%% set_prolog_flag(F,X): sets Prolog flag
set_prolog_flag(F,X):-F<==X.

%% current_prolog_flag(F,X): sets Prolog flag

current_prolog_flag(dialect,X):-is_prolog(X).
current_prolog_flag(version_data,X):-is_prolog(F),version_data(Xs),X=..[F|Xs].
current_prolog_flag(F,X):-gvar(F),F==>X.

multifile(FN):-dynamic(FN).
discontiguous(FN):-dynamic(FN).

meta_predicate(_).

match_before(Stop,Cs)-->match_before([Stop],Cs,_).

match_before(Stops,[],Stop)-->[Stop],{member(Stop,Stops)},!.
match_before(Stops,[C|Cs],Stop)-->[C],match_before(Stops,Cs,Stop).

%% set_encoding(Encoding): sets char encoding
set_encoding(Encoding):-
  call_java_class_method('vm.logic.Interact',setEncoding(Encoding),_).

%% get_encoding(Encoding): gets char encoding
get_encoding(Encoding):-
  call_java_class_method('vm.logic.Interact',getEncoding,Encoding).

force_encoding_of(S,Bad,Good, NewS):-
  call_java_class_method('vm.logic.Interact',forceEncodingOf(S,Bad,Good),NewS).

mac2utf(S,NewS):-force_encoding_of(S,'MacRoman','UTF-8', NewS).

win2utf(S,NewS):-force_encoding_of(S,'ISO-8859_1','UTF-8', NewS).

not_null(A):-var(A),!.
not_null('$null'):-!,fail.
not_null(_).
       
% backward compatibility box

% list50test(Xs):-call_java_class_method('vm.logic.TermConverter',bigList,Xs).

%% log(Mes): sends Mes + timestamp (usually something wrong) to the logger
log(Mes):-call_java_class_method('vm.logic.Interact',log(Mes),_).


log_to(Fname,Mes):-log_to(Fname,Mes,1).

%%  log_to(Fname,Mes,WithStamp): if WithTimeStamp>0 appends <<< tamp >>> + Mes to Fname
log_to(Fname,Mes,WithStamp):-
  call_java_class_method('vm.logic.Interact',log_to(Fname,Mes,WithStamp),_).
  
%% get_date_time(DateString): gets current date and time as formatted string

get_date_time(DateString):-call_java_class_method('vm.logic.Interact',getDateTime,DateString).

%% get_time(TimeFrom1970ms): get time from 1970 Jan 1 in miliseconds
get_time(TimeFrom1970ms):-call_java_class_method('vm.logic.Interact',getTime,TimeFrom1970ms).

symtest:-
  M is 1<<22,
  for(I,0,M),
  atom_concat(a,I,_),
  fail.
symtest.

mmaptest:-
  MI is 1000,
  MJ is 100,
  time(mmaptest(MI,MJ),T),
  R is MI*MJ*3,
  println(perf(ops(R),time(T))).

mmaptest(MI,MJ):-
  new_mmap(D),
  mmaptest1(D,MI,MJ),
  mmaptest2(D,MI,MJ),
  mmaptest3(D,MI,MJ).
  
mmaptest1(D,MI,MJ):-
  for(I,0,MI),
    for(J,0,MJ),
      mmap_put(D,I,J),  
  fail.
mmaptest1(_,_,_).

mmaptest2(D,MI,_):-
  for(I,0,MI),
      mmap_get(D,I,_),  
  fail.
mmaptest2(_,_,_).

mmaptest3(D,MI,MJ):-
  for(I,0,MI),
    for(J,0,MJ),
      mmap_remove(D,J,I),  
  fail.
mmaptest3(_,_,_).

%% new_mmap(D): creates a new multi-map dictionary with one or more values associated to each key
new_mmap(D):-
  call_java_class_method('vm.extensions.MMap',create,D).

%% mmap_put(D,X,A): adds A to key X in multi-map D
mmap_put(D,X,A):-nonvar(X),nonvar(A),!,invoke_java_method(D,put(X,A),_).
mmap_put(D,X,A):-errmes(instantiation_error,mmap_put(D,X,A)).

%% mmap_get(D,X,A): returns an A associated to key X in multi-map D - backtracks if X unbound
mmap_get(D,X,A):-nonvar(D),nonvar(X),
   !,
   invoke_java_method(D,valueIterator(X),I),ielement_of(the(I),A).
mmap_get(D,X,A):-nonvar(D),!,
   invoke_java_method(D,keyIterator,KI),
   ielement_of(the(KI),X),
   invoke_java_method(D,valueIterator(X),I),
   ielement_of(the(I),A).
mmap_get(D,X,A):-errmes(instantiation_error,mmap_get(D,X,A)).

%% mmap_remove(D,X,A): removes specific A associated to key X in multi-map D
mmap_remove(D,X,A):-nonvar(D),nonvar(X),!,invoke_java_method(D,remove(X,A),_).
mmap_remove(D,X,A):-errmes(instantiation_error,mmap_remove(D,X,A)).

%% mmap_remove(D,X): removes all values associated to key X in multi-map D
mmap_remove(D,X):-nonvar(D),nonvar(X),!,invoke_java_method(D,remove(X),_).
mmap_remove(D,X):-errmes(instantiation_error,mmap_remove(D,X)).

% end

  
