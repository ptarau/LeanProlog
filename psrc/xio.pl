% xio.pl

%% get_system_property(Name,Prop): gets system proprty - if var(Name) it backtracks over all

get_system_property(Name,Prop):-nonvar(Name),!,get_system_property0(Name,Prop).
get_system_property(Name,Prop):-get_system_property1(Name,Prop).

get_system_property0(Name,Prop):-call_java_class_method('java.lang.System',getProperty(Name),Prop).

set_system_property(Name,Prop):-call_java_class_method('java.lang.System',setProperty(Name,Prop),_).

get_system_property1(Name,Value):-
  call_java_class_method('compat.Tools',props,Iterator),
  ielement_of(the(Iterator),Name),
  get_system_property0(Name,Value).

%% os_name(OS): return OS
os_name(OS):-get_system_property0('os.name',OS).

%% user_home(Dir): returns user's home directory
user_home(Dir):-get_system_property0('user.home',Dir).

%% working_directory(Old,New): binds Old to current directory and changes it to New    
working_directory(Old,New):-
 get_working_directory(Old), % moved to xbuiltins
 set_working_directory(New).
 
%% chdir(Dir), cd(Dir): changes (globally) current directory to Dir
chdir(Dir):-set_working_directory(Dir).
cd(Dir):-set_working_directory(Dir).

%% open(File,Mode,Stream): opens a file in given mode, returning a stream
open(File,Mode,Stream):-openable(File),nonvar(Mode),Mode=read,!,
  new_interactor('vm.extensions.LineStreamInput',File,Stream).
open(File,Mode,Stream):-openable(File),nonvar(Mode),Mode=write,!,
  new_interactor('vm.extensions.LineStreamOutput',File,Stream).  
open(File,Mode,Stream):-openable(File),nonvar(Mode),Mode=append,!,
  atom_concat('++',File,AppFile),
  new_interactor('vm.extensions.LineStreamOutput',AppFile,Stream).    
open(File,Mode,_Stream):-
  errmes('bad arguments',open(File,Mode)).  

openable(File):-atom(File),!.
openable(Stream):-interactor(Stream).
 
%%  close(Stream): closes stream 

close(Stream):-openable(Stream),!,stop_interactor(Stream).
close(Stream):-errmes('bad arguments',close(Stream)).  

%% set_alias(Stream,Alias): binds Stream and Alias together
%% get_alias(Stream,Alias): when at least one is nonvar it returns the other
%% remove_alias(StreamOrAlias) -- removes the alias if the stream or the alias are given
  
set_alias(Stream,Name):-
  call_java_class_method('vm.extensions.IOInteractor',set_alias(Stream,Name),_).
  
remove_alias(StreamOrName):-
  call_java_class_method('vm.extensions.IOInteractor',remove_alias(StreamOrName),_).
  
get_alias(Stream,Alias):-interactor(Stream),!,
  call_java_class_method('vm.extensions.IOInteractor',stream2alias(Stream),the(Alias)).
get_alias(Stream,Alias):-nonvar(Alias),!,
  call_java_class_method('vm.extensions.IOInteractor',alias2stream(Alias),the(Stream)).
get_alias(Stream,Alias):-errmes('bad arguments',get_alias(Stream,Alias)).    

get_initiator(Stream,FName):-invoke_java_method(Stream,getInitiator,FName).

get_reader(Stream,Source):-interactor(Stream),!,jcall('vm.extensions.LineStreamInput',getReader(Stream),Source).
get_reader(Stream,Source):-errmes(bad_arguments,get_reader(Stream,Source)).

get_tokenizer(Stream,Tokenizer):-invoke_java_method(Stream,getTokenizer,Tokenizer).

%% current_input(Stream) : returns current input stream
current_input(Stream):-get_val(current_input,S),!,Stream=S.
current_input(Stream):-
  %traceln('no current input!!!'),
  stdio(Stream).

%% set_input(Stream) : sets current input stream
set_input(Stream):-interactor(Stream),!,set_val(current_input,Stream).
set_input(Stream):-errmes(bad_arguments,set_input(Stream)).

%% current_output(Stream): returns current output stream
current_output(Stream):-get_val(current_output,S),!,Stream=S.
current_output(Stream):-
  %traceln('no current output!!!'),
  stdio(Stream).

%% set_output(Stream): sets current output stream
set_output(Stream):-interactor(Stream),!,set_val(current_output,Stream).
set_output(Stream):-errmes(bad_arguments,set_output(Stream)).

ttyprint(X):-stdio(S),println(S,X).
ttynl:-stdio(S),nl(S).

%% println(T): prints term T + nl to cirrent output
println(X):-current_output(I),println(I,X).

%xpp(X):-current_output(I),atomic_writeln(I,X).

println(Stream,X):-atomic_writeln(Stream,X).
%println(Stream,X):-sync_println(Stream,X).

/*
sync_println(X):-current_output(I),sync_println(I,X).

% sync_println(_,X):-traceln('HERE:'(X)),fail.
sync_println(Stream,X):-
  'i_printer'==>S,
  not_null(S),
  !,
  i_call(S,writeln(Stream,X)).
sync_println(Stream,X):-
  i_server(S),
  'i_printer'<==S,
  i_call(S,writeln(Stream,X)).
*/

writeln(X):-current_output(I),writeln(I,X).
       
writeln(Stream,X):-
  write(Stream,X),
  nl(Stream).
  
write(T):-
  current_output(I),
  write(I,T).

atomic_writeln(I,T):-atomic_writeln0(I,T),fail.
atomic_writeln(_,_).

atomic_writeln0(I,T):-
  atom_codes(NL,[10]),
  numbervars(T,0,_), % remember to fail !!!
  unparse(T,Xs,[NL]),
  atomic_list_concat(Xs,X),
 
  fwrite(I,X),
  !.
atomic_writeln0(_,T):-
  traceln('FAILING_TO_WRITE'(T)).
  
    
%fix_VAR(X,V):-compound(X),arity(X,1),arg(1,X,I),integer(I),!,atom_concat('_',I,V).
%fix_VAR(X,X).
  
%% write(Stream,Term): writes term to stream
write(I,T):-
  numbervars(T,0,_),
  unparse(T,Xs),
    member(X,Xs),
    fwrite(I,X),
  fail.
write(_,_).

fwrite(I,T):-
  tell_interactor(I,'$cmd'(2,T),_), % WRITE
  ask_interactor(I,_).

fwriteln(I,T):-fwrite(I,T),fnl(I).

fnl(I):-tell_interactor(I,'$cmd'(3),_),ask_interactor(I,_). % NL

%% nl(Stream): writes newline to stream    
nl(I):-fnl(I).
nl:-current_output(I),nl(I).

tab(N):-current_output(I),tab(I,N).

%% tab(Stream,N): writes N spaces to Stream 
tab(I,N):-between(1,N,_),fwrite(I,' '),fail;true.

%% put_code(Stream,C): write character of given code to stream
put_code(I,C):-tell_interactor(I,'$cmd'(4,C),_),ask_interactor(I,_). % PUT_CODE
put_code(C):-current_output(I),put_code(I,C).

write_quoted(X):-current_output(S),write_quoted(S,X).

% limited to atoms
write_quoted(S,X):-atomic(X),[Q]="\'",put_code(S,Q),write(S,X),put_code(S,Q).
write_quoted(S,X):-write(S,X).


%% tell(File), told, telling(File) : direct output to given file (deprecated)
tell(File):-
  (interactor(File)->Stream=File;open(File,'write',Stream)),
  set_output(Stream).
  
told:-
  current_output(Stream),
  close(Stream),
  remove_val(current_output).

telling(Stream):-current_output(Stream).  

put(F,C):-put_code(F,C).
put(C):-put_code(C).

get(C):-get_code(C).

get0(C):-get_code(C).
get0(F,C):-get_code(F,C).

get_char(X):-get_code(C),char_code(X,C).

get_char(F,X):-get_code(F,C),char_code(X,C).

statistics(runtime,[T,0]):-cputime(X),T is X*1000.
statistics(symbols,X):-symbols(X).
statistics(engines,X):-engines(X).
statistics(heapsizes,X):-heapsizes(X).
statistics(choice_point_stack_sizes,X):-stacksizes(X).
statistics(trailsizes,X):-trailsizes(X).
statistics(flags,X):-flags(X).
statistics(max_memory_MBytes,X):-max_memory(X).
statistics(used_memory_MBytes,X):-used_memory(X).
statistics(available_memory_MBytes,X):-available_memory(X).
statistics(thread_count,X):-thread_count(X).
statistics(max_cores,X):-max_cores(X).
statistics(db_size,X):-db_size(X).
statistics(xprofile,X):-xprofile(X).

%% write_codes(Cs): write each char represented by codes in Cs to stream
write_codes(Cs):-current_output(S),write_codes(S,Cs).

write_codes(_,[]).
write_codes(S,[C|Cs]):-put_code(S,C),write_codes(S,Cs).

%% stdio(I): returns standard input/output - an interactor usable as both streams 
stdio(I):-new_interactor('vm.extensions.ConsoleIO','?- ',I). % with jline line editing
%stdio(I):-new_interactor('vm.extensions.SimpleConsoleIO','?- ',I).


% parser.pl interface

%term_of(File,Term):-xterm_of(File,Term). % memory issues on large files

% we also handle here includes, operators, dynamics, indexing etc.
% use term0_of - if none of the above are needed

%% term_of(File,Term): scans recursively over files following ':-[File].' commands for terms
term_of(File,Term):-term_of(File,consulting,Term).

term_of(File,Mode,Term):-rterm_of(File,Mode,Term).
term_of(_,_,_):-clear_all_loaded,fail.

term0_of(File,Term):-
 find_file(File,PathFile),
 term0_of(PathFile,Term,_Vs,_L1,_L2).

%% included_file_of(File,Included): iterates over all files included with statemts of the form :-[F1,...Fn] in File
included_file_of(File,IncludedF):-
  term0_of(File,':-'(Is)),
  nonvar(Is),
  ( Is=include(Included)
  ; member(Included,Is)
  ),
  expand_env(Included,IncludedF).
  
term0_of(PathFile,Term,Vs,L1,L2):-
  clause_tokens_of(yes,PathFile,Ts,Vs,L1,L2),
  parse_tokens(Ts,Vs,L1,L2,Term).

parse_tokens(Ts,Vs,Term):-
  parse_tokens(Ts,Vs,1,1,Term).

%% parse_tokens(Ts,Vs,L1,L2,Term): parses tokens into term writes out syntax eror from L1 to L2 if fails to 
parse_tokens(Ts,Vs,L1,L2,Term):-
  Ts\=[],
  toks2term(Ts,Term),
  maybe_syntax_error(Term,Vs,L1,L2).


maybe_syntax_error(Term,Vs,L1,L2):-
  nonvar(Term),
  Term=syntax_error(Ts),
  !,
  maplist(unvar,Vs),
  untok(Ts,Xs,[]),
  concat_atom(['*** '|Xs],Y),
  traceln(''),
  concat_atom(['*** SYNTAX_ERROR BETWEEN LINES: ',L1,'-',L2],E),
  traceln(E),
  traceln(Y),
  traceln(''),
  log(E),
  log(Y),
  get_quickfail(QuickFail),
  QuickFail>3,
  throw(prolog_syntax_error), % might get printed ...
  fail.
maybe_syntax_error(_,_,_,_).

rterm_of('$str'(String),Mode,EC):-!,
  term_of_string(String,Term),
  handle_file_term(Term,'',Mode,EC).
rterm_of(File,Mode,Term):-
 find_file(File,PathFile),
 rterm0_of(PathFile,Mode,Term).
 
rterm0_of(PathFile,Mode,EC):-
  traceln(opening_file(PathFile,Mode)),
  abs2path_file(PathFile,Path,_File),
  term0_of(PathFile,Term,_Vs,_L1,_L2),
  handle_file_term(Term,Path,Mode,EC).
  
handle_file_term(Term,Path,Mode,EC):-
  ( Term=':-'(G)->nonvar(G),handle_file_goal(G,Path,Mode,EC) % !!!
  ; handle_file_clause(Mode,Term,EC) % !!!
  ).
  
handle_file_goal([F0|Fs],Path,Mode,EC):-!,
  member(F,[F0|Fs]),
  handle_included_file(F,Path,Mode,EC).
handle_file_goal(include(F),Path,Mode,EC):-!,
  handle_included_file(F,Path,Mode,EC).
handle_file_goal(ensure_loaded(F),Path,Mode,EC):-!,
  remember_loaded(F),
  build_file_name(Path,F,PF),
  remember_loaded(PF),
  rterm0_of(PF,Mode,EC). 

handle_file_goal(op(X,Y,Z),_Path,Mode,_EC):-!,
  op(X,Y,Z),
  schedule_at_runtime(Mode,to_wcompile(op(X,Y,Z))),  % for wcompile,mcompile
  fail.
handle_file_goal(dynamic(FN),_Path,Mode,_EC):-!,
  dynamic(FN), % for consult
  schedule_at_runtime(Mode,to_wcompile(dynamic(FN))), % for wcompile,mcompile
  fail.
handle_file_goal(index(X),_Path,Mode,_EC):-!,
  index(X), % for consult
  schedule_at_runtime(Mode,to_wcompile(index(X))), % for wcompile,mcompile
  fail.
handle_file_goal(multifile(FN),_Path,Mode,_EC):-!, % FOR NOW multifile is interpreted as dynamic !!!
  dynamic(FN), % for consult
  schedule_at_runtime(Mode,to_wcompile(dynamic(FN))), % for wcompile,mcompile
  fail.
handle_file_goal(discontiguous(FN),_Path,Mode,_EC):-!, % FOR NOW discontiguous is interpreted as dynamic !!!
  dynamic(FN), % for consult
  schedule_at_runtime(Mode,to_wcompile(dynamic(FN))), % for wcompile,mcompile
  fail.   
handle_file_goal(initialization(G),_Path,Mode,_EC):-Mode==compiling,!, 
  schedule_at_runtime(Mode,initialization(G)), % for wcompile,mcompile
  fail.   
% add more handlers here 
 handle_file_goal(G,_Path,Mode,_EC):-Mode==compiling,!,
  schedule_at_runtime(Mode,initialization(G)),
  fail.  
 handle_file_goal(G,_Path,Mode,_EC):-Mode==consulting,!,
  catch(topcall(G),R,traceln('*** ERROR IN :-(GOAL)'(G,R))),
  fail.  

handle_included_file(F,Path,Mode,EC):-
  build_file_name(Path,F,PF),
  rterm0_of(PF,Mode,EC).  

handle_file_clause(compiling,C,_):-C=(H:-_),
  is_dynamic(H),!,
  schedule_at_runtime(compiling,to_wcompile(assert(C))),
  fail.
handle_file_clause(compiling,H,_):-
  is_dynamic(H),!,
  schedule_at_runtime(compiling,to_wcompile(assert(H))),
  fail.
handle_file_clause(_,C,C).

build_file_name(_Path,F,PF):-absolute_file_name(F,AF),exists_file(AF),!,PF=AF.
build_file_name(Path,F,PF):-
  atom_concat(Path,F,PF0),
  find_file(PF0,R),
  !,
  PF=R.

schedule_at_runtime(consulting,_):-!.
schedule_at_runtime(compiling,Action):-db_assert('$$',Action).

remember_loaded(F):-db_clause('$loaded',deja_vu(F),true),!,fail.
remember_loaded(F):-db_assert('$loaded',deja_vu(F)).

clear_all_loaded:-db_clear('$loaded').

initialization(G):-
  %traceln('>>>initialization'(G)),
  topcall(G),
  !,
  fail.
initialization(G):-
  warnmes(failed,initialization(G)).  

% flexible but inefficient

xterm_of(File,Term):-
 find_file(File,PathFile),
 file_term_of(PathFile,Term).
   
file_term_of(PathFile,EC):-
  traceln(opening_file(PathFile)),
  abs2path_file(PathFile,Path,_File),
  xterms_of(PathFile,Cs,Fs,_Gs,_Ps),
  ( member(C,Cs),
    C=EC % expand_term(C,EC)
  ; member(F,Fs),
    build_file_name(Path,F,PF),
    file_term_of(PF,EC)
  ).
  
xterms_of(PathFile,Cs,Fs,Gs,Ps):-
   findall(T,term0_of(PathFile,T),Ts),
   extract_terms(Ts,Cs,Fs,Gs,Ps).

extract_terms([],[],[],[],[]).
extract_terms([':-'([F])|Ts],Cs,[F|Fs],Gs,Ps):-!,
  extract_terms(Ts,Cs,Fs,Gs,Ps).
extract_terms([':-'(ensure_loaded(F))|Ts],Cs,[F|Fs],Gs,Ps):-!,
  extract_terms(Ts,Cs,Fs,Gs,Ps).  
extract_terms([':-'(op(X,Y,Z))|Ts],Cs,Fs,Gs,Ps):-!,
  op(X,Y,Z), % execute now
  extract_terms(Ts,Cs,Fs,Gs,Ps).  
extract_terms([':-'(G)|Ts],Cs,Fs,[G|Gs],Ps):-!,
  % execute later
  extract_terms(Ts,Cs,Fs,Gs,Ps).  
extract_terms([T|Ts],[T|Cs],Fs,Gs,[P/N|Ps]):-
  to_clause(T,C),C=(H:-_),functor(H,P,N),
  extract_terms(Ts,Cs,Fs,Gs,Ps).

  
clause_tokenizer(File,Stream):-
  new_interactor('vm.extensions.TokenStreamInput',File,Stream).



%% term_to_atom(T,A): writes out term T to atom A
term_to_atom(T,A):-var(T),nonvar(A),!,atom_to_term(A,T,_).
term_to_atom(T0,A):-
  copy_term(T0,T),numbervars(T,0,_),
  unparse(T,Xs),
  unspace(Xs,Us),
  concat_atom(Us,A).

%% atom_to_term(A,T): reads term T from atom A
atom_to_term(S,Term):-atom_to_term(S,Term,_).

%% atom_to_term(A,T,Vs): reads term T + vars Vs from atom A
atom_to_term(S,Term,Vs):-
  atom(S),
  !,
  goal_tokens_of(S,Ts,Vs),
  parse_tokens(Ts,Vs,Term).
atom_to_term(S,_Term,_Vs):-
  errmes(atom_expected,atom_to_term(S)).
  
call_goal_string(StringGoal,Vs):-
  atom_to_term(StringGoal,Term,Vs),
  once(Term).

term_of_string(S,T):-term_of_string(S,T,_).

%% term_of_string(S,T,Vs): backtracks over all terms T with vars Vs read from string S
term_of_string(S,T,Vs):-
  clause_tokens_of(S,Ts,Vs),
  parse_tokens(Ts,Vs,T).
  
goal_tokens_of(String,Ts,Vs):-
  clause_tokens_of(no,'$str'(String),Ts,Vs,_Line1,_Line2).
     
clause_tokens_of(String,Ts,Vs):-
  clause_tokens_of(yes,'$str'(String),Ts,Vs,_Line1,_Line2).
  
clause_tokens_of(Show,File,Ts,Vs,Line1,Line2):-
  clause_tokenizer(File,T),
  element_of(T,Between),
  process_toks(Show,Between,Ts,Vs,Line1,Line2).
 
process_toks(Show,between(Line1,Line2,Ts,VsOccs),Ts,Vs,Line1,Line2):-
  Ts=[First|_],
  extract_vars(VsOccs,Vs,Singles,[]), % RECLIMIT
  (Show=yes->showSingleVars(Singles,First,Line1,Line2);true).


showSingleVars(Vs,Pred,L1,L2):-
  Vs=[_|_],
  get_verbosity(Verbosity),
  Verbosity>1,
  !, % compiler_bug without this !!!
  Line is (L1+L2) // 2,
  traceln('*** SINGLE_OCCURENCE_VARIABLES_IN'(Pred,line(Line),Vs)).
showSingleVars(_,_,_,_).
  
extract_vars([],[])-->[].
extract_vars([var(Name,Var,Occ)|Xs],[Name-Var|Vs])-->
   addSingle(Occ,Name),
   extract_vars(Xs,Vs).

addSingle(0,Name)-->
  [Name],
  {[Under]="_",atom_codes(Name,[C|_]),C =\= Under},
  !.
addSingle(_,_)-->[].


read_words(R):-current_input(I),read_words(I,R).

%% read_words(Stream,Words): reads words from stream
read_words(I,R):-
  tell_interactor(I,'$cmd'(6),_), % READWORDS
  ask_interactor(I,R). % R= list of words on a line

word_reader(File,Stream):-
  new_interactor('vm.extensions.WordStreamInput',File,Stream).

word_of_string(S,W):-
  word_of0('$str'(S),W).

%% words_of(File,Words): converts a File to a list of words
words_of(File,Ws):-findall(W,word_of(File,W),Ws).

%% word_of(File,W): backtracks over words found in in a file, one at time
word_of(File,W):-
   find_file(File,F),
   word_of0(F,W).
   
word_of0(F,W):-
  word_reader(F,R),
  element_of(R,W).
 
%% codes_words(Cs,Ws): parses/unparses list of codes to/from words    
codes_words(Cs,Ws):-var(Ws),!,
  atom_codes(S,Cs),
  findall(W,word_of_string(S,W),Ws).
codes_words(Cs,Ws):-
  findall(C,words_code(Ws,C),Cs).

/*
add_stop_to("."," .",['.']):-!.
add_stop_to([]," .",[]):-!.
add_stop_to([C|Cs],[C|Ds],End):-add_stop_to(Cs,Ds,End).
*/


%% words_code(Ws,C): backtracks over all char codes in a list of words
words_code(Ws,C):-words2nat(Ws,Ns),member(N,Ns),name(N,Xs),member(C,Xs).

%% words2sentence(Ws,Sentence): combines words with spaces as needed for a better looking natural language sentence
words2sentence(Ws,Sentence):-
 words2nat(Ws,Ns),
 atomic_list_concat(Ns,Sentence).

%% words2nat(Ws,Ns): interperses words with spaces while making punctuation look natural
words2nat(Ws,Ns):-words2nat(Wss,Ws,[]),append(Wss,Vs),once(append(Us,[S],Vs)),(' '=S->Ns=Us;Ns=Vs).

words2nat([])-->[].
words2nat([[W,C,' ']|Wss])-->[W,C],{left_collider(C)},!,words2nat(Wss).
words2nat([[L,C]|Wss])-->[L,C],{collider(C)},!,words2nat(Wss).
%words2nat([[Q,W,Q,' ']|Wss])-->[Q],{Q=('"')},[W],[Q],!,words2nat(Wss).
words2nat([[Q|Vs],[Q]|Wss])-->[Q],{Q=('"')},match_before(Q,Ws),!,{words2nat(Ws,Vs)},words2nat(Wss).
words2nat([[W,' ']|Wss])-->[W],words2nat(Wss).

left_collider(W):-member(W,[(','),(':'),(';'),('.'),('!'),('?')]).

collider(W):-member(W,[(''''),('-')]).

%% token_of(File,T): backtracks over all tokens in given string
token_of_string(String,T):-token0_of('$str'(String),T).

%% token_of(File,T): backtracks over all tokens in File
token_of(File,T):-
  find_file(File,F),
  token0_of(F,T).
  
token0_of(F,T):-
  clause_tokens_of(no,F,Ts,Vs,_L1,_L2),
  bind_var_tokens(Vs),
  %member(T0,Ts),trim_fun_token(T0,T).
  terminate_tokens_of(Ts,T).
  
terminate_tokens_of(Ts,T):-member(T,Ts).
terminate_tokens_of([_|_],end_of(clause)).
  
bind_var_tokens([]).
bind_var_tokens([N-N|Ts]):-bind_var_tokens(Ts).

/*
trim_fun_token(T,X):-compound(T),!,arg(1,T,X).
trim_fun_token(T,T).
*/

%% readln(T): reads line from current input stream
readln(T):-
  current_input(I),
  readln(I,T).
  
readln(I,T):-
  % traceln('INTER'(I)),
  readln_codes(I,R),
  ( R=the(Cs)->atom_codes(T,Cs)
  ; T='end_of_file.'
  ).

readln_codes(R):-
  current_input(I),
  readln_codes(I,R).
  
readln_codes(I,R):-
  % from_codes("$readln",X), % avoids reserved word confusion in self-compilation
  tell_interactor(I,'$cmd'(0),_), % READLN
  % can be anything about what we would like the data format to be
  ask_interactor(I,R). % R= the(Cs) or atom no

%% read_string(S): reads from current input, a line as a symbol without printing any prompt
read_string(T):-
  current_input(I),
  read_string(I,T).

%% read_string(I,S): reads from interactor I, a line as a symbol without printing any prompt 
read_string(I,T):-prompt_and_readln_codes(I,'',R),
 ( R=the(Cs)->atom_codes(T,Cs)
  ; T='end_of_file.'
  ).

prompt_and_readln_codes(Prompt,Cs):-
   current_input(I),
   prompt_and_readln_codes(I,Prompt,R), 
   R=the(Cs).
    
prompt_and_readln_codes(I,Prompt,R):-
  % from_codes("$readln",X), % avoids reserved word confusion in self-compilation
  tell_interactor(I,'$cmd'(0,Prompt),_), % READLN
  % can be anything about what we would like the data format to be
  ask_interactor(I,R). % R= the(Cs) or atom no

%% get_code(C): reads a char code from current input stream
get_code(R):-
  current_input(I),
  get_code(I,R).
  
%% get_code(I,C): reads a char code from interactor/input stream I
get_code(I,C):-
  tell_interactor(I,'$cmd'(5),_), % GET_CODE
  ask_interactor(I,C).

/*
% work only with terms ending with "." on single lines
%read(T):-read_term(T).
%read_term(Term):-readln_goal(Term,_).
%read_term(T,Vs):-readln_goal(T,Vs).
*/

readln_goal(Term,Vs):-
  current_input(I),
  readln_goal(I,Term,Vs).

readln_goal(I,Term,Vs):-
  read_toks(I,Between),
  process_toks(no,Between,Ts,Vs0,Line1,Line2),
  parse_tokens(Ts,Vs0,Line1,Line2,Term0),
  !,
  Term=Term0,
  Vs=Vs0.
  
   
read_toks(R):-current_input(I),read_toks(I,R).

read_toks(I,R):-
  % from_codes("$readtoks",X), % avoids reserved word confusion in self-compilation
  tell_interactor(I,'$cmd'(1),_), % READTOKS
  % can be anything about what we would like the data format to be
  ask_interactor(I,R). % R= between(L1,L2,Ts,Vs)


%% read_term(Term,Options): reads term from current input according to Options - e.g [singleton(warning)]
read_term(Term,Options):-current_input(Stream),read_term(Stream,Term,Options).

%% read_term(Stream,Term,Options): reads term from Stream according to Options - e.g [singleton(warning)]
read_term(Stream,Term,Options):-
  nonvar(Options),
  ( member(singletons(warning),Options)->
    Warn=yes
  ; Warn=no
  ),
  read(Stream,Warn, Term,_Vs).
  
%% read(Term): reads term from current input - a line at time when from console
read(Term):-
  current_input(I),
  ( stdio(J),J==I,!->write('>>'),readln_goal(I,Term,_)
  ; read(I,Term)
  ).


%% read(Stream,Term): reads term from Stream
read(Stream,Term):-read(Stream,no,Term,_Vs).

%% read(Stream,WarnSingletons, Term,Vs): reads term from Stream as well as list of variables with names
read(Stream,WarnSingletons, Term,Vs):-read_term(Stream,WarnSingletons, Term,Vs,_Line1,_Line2).
 

%% read_term(Stream,WarnSingletons, Term,Vs,Line1,Line2): reads a term from a stram and if WarnSingletons=yes warns accordingly

read_term(I,_WarnSingletons, Term,Vs,0,0):-stdio(J),J==I,!,write('>>'),readln_goal(J,Term,Vs).
read_term(Stream,WarnSingletons, Term,Vs,Line1,Line2):-
  get_tokenizer(Stream,Tok),
  get(Tok,the(Between)),
  !,
  process_toks(WarnSingletons,Between,Ts,Vs,Line1,Line2),
  parse_tokens(Ts,Vs,Line1,Line2,Term).
read_term(_Stream,_,end_of_file,[],0,0).
  
    
% term_of ...
 
codes_of(File,Cs):-findall(C,code_of(File,C),Cs).


%% code_of(File,C): backtracks over char codes in File
code_of(File,C):-line_codes_of(File,Cs),line2code(Cs,C).

%% line_of(File,C): backtracks over lines in File
line_of(File,L):-line_codes_of(File,Cs),atom_codes(L,Cs).

%% line_codes_of(File,Cs): backtracks over lines seen as lists of char codes in File
line_codes_of(File0,Cs):-
  find_file(File0,File),
  open(File,read,S),
  repeat,
  ( readln_codes(S,Line),Line=the(Cs0)->Cs=Cs0
  ; !,
    close(S),
    fail
  ).
  
line2code(Cs,C):-member(C,Cs).
line2code(_,10).

find_prolog_file(FileDescr,NewFile):-
  SuffixesCss=["",".pl",".pro",".wam"],
  find_file(FileDescr,SuffixesCss,NewFile).
  
find_file(FileDescr,NewFile):-
  SuffixesCss=["",".pl",".pro"],
  find_file(FileDescr,SuffixesCss,NewFile).


get_path(Pss):-path_elements(Ps),maplist(atom_codes,Ps,Pss).

%% path_element(P): backtracks over path elements P
path_element(P):-path_elements(Ps),member(P,Ps).

find_file(FileDescr,Sufs,NewFile):-
   get_path(Ps),
   append(["","progs"],Ps,Prefs),
   find_file(Prefs,FileDescr,Sufs,NewFile).

%% find_file(Prefs,File,Sufs,NewFile): finds File with name prefixed and suffixed as NewFile
    
find_file(_,File,_,NewFile):-
  absolute_file_name(File,AFile),
  exists_file(AFile),
  !,
  NewFile=AFile.
find_file(Prefs,File,Sufs,NewFile):-
  atom_codes(File,Fs),
  member(Suf,Sufs),
  member(Pref0,Prefs),
  expand_pref(Pref0,Pref),
  append([Pref,Fs,Suf],Cs),
  atom_codes(NewFile0,Cs),
  % traceln(fileCandidate(NewFile)),
  absolute_file_name(NewFile0,NewFile1),
  exists_file(NewFile1),
  !,
  NewFile=NewFile1.
find_file(_Prefs,File,_Sufs,_NewFile):-
  errmes(file_not_found,File).
  
expand_pref([],Cs):-!,Cs=[].
expand_pref(Cs,NewCs):-nonvar(Cs),append(Cs,"/",NewCs).

%% abs2path_file(Abs,Path,File): splits an absolute file name into Path and File
abs2path_file(Abs,Path,File):-
  atom_codes(Abs,Cs),reverse(Cs,RCs),
  % [Sep1,Sep2]="/\",
  [Sep1,Sep2]=[47,92],
  ( append(RFs,[Sep1|Xs],RCs)->RPs=[Sep1|Xs]
  ; append(RFs,[Sep2|Xs],RCs)->RPs=[Sep2|Xs]
  ; RPs=[],RFs=RCs
  ),
  reverse(RFs,Fs),
  reverse(RPs,Ps),
  !,
  atom_codes(File,Fs),
  atom_codes(Path,Ps),
  % traceln(exiting_abs2path_file(Abs,Path,File)),
  true.
  
  
% pretty print

portray_clause(S,C):-
  current_output(Old),
  set_output(S),
  portray_clause(C),
  set_output(Old).
 
pp(C):-portray_clause(C).
  
portray_clause(C):-numbervars(C,0,_),pp_clause0(C),fail.
portray_clause(_).

pp_clause0(C):-var(C),!,top_writeq_atom(C).
pp_clause0(:-(Body)) :- !,
        nl,
        l_clauses(Body, 0, 2, 8).
pp_clause0((Pred:-Body)) :-
        top_writeq_atom(Pred),
        l_clauses(Body, 0, 2, 8), !.
pp_clause0((Pred)) :-
        pp_clause0((Pred:-true)).

l_clauses(C, _L, _R, _D):-var(C),!,top_writeq_atom(C).
l_clauses((A,B), L, R, D) :- !,
        l_clauses(A, L, 1, D), !,
        l_clauses(B, 1, R, D).
l_clauses(true, _, 2, _) :- !,[P]=".",
        put_code(P), nl.
l_clauses((A;B), L, R, D) :- !,
        l_magic(fail, L, D),
        l_magic((A;B), 0, 2, D),
        l_magic_nl(R, '.').
l_clauses((A->B), L, R, D) :- !,
        l_clauses(A, L, 5, D), !,
        l_clauses(B, 5, R, D).
l_clauses(Goal, L, R, D) :-
        l_magic(Goal, L, D),
        top_writeq_atom(Goal),
        l_magic_nl(R,'.').

l_magic(!,    0, _) :- !,pp_write(' :- ').
l_magic(!,    1, _) :- !,pp_write(',  ').
l_magic(_, 0, D) :- !,
        pp_write(' :- '),
        nl, tab(D).
l_magic(_Goal, 1, D) :- !,
        [Char]=",",
        put_code(Char),
        nl, tab(D).
l_magic(_, 3, _) :- !,pp_write('(   ').
l_magic(_, 4, _) :- !,
        pp_write(';   ').
l_magic(_, 5, D) :- !,
        pp_write(' ->'),
        nl, tab(D).
l_magic(_, Key, D) :-
        atom(Key),
        pp_write((':- ')), pp_write(Key),
        nl, tab(D).

l_magic_nl(2, C) :- !, pp_write(C),nl.
l_magic_nl(_, _).

l_magic((A;B), L, R, D) :- !,
        l_magic(A, L, 1, D), !,
        l_magic(B, 1, R, D).
l_magic(Conj,  L, R, D) :-
        E is D+8,
        M is L+3,
        l_clauses(Conj, M, 1, E),
        nl, tab(D),
        l_magic2(R, ')' ).

l_magic2(2, C) :- !, pp_write(C).
l_magic2(_, _).

pp_write(C):-write(C).

top_writeq_atom(X):-writeq(X).

%% writeq(T): writes out T while quoting objects in int when needed
writeq(T):-current_output(O),writeq(O,T).

writeq(F,T):-object_to_quoted_string(T,S),fwrite(F,S).


%% write_canonical(Stream,Term): writes a term to a stream such that it can be read back
write_canonical(O,T):-to_rstring(T,S),fwrite(O,S).

%% write_canonical(Term): writes tu current output a term such that it can be read back
write_canonical(T):-current_output(O),write_canonical(O,T).

%% display(Term): displays a term in a canonical form
display(X):-write_canonical(X).


