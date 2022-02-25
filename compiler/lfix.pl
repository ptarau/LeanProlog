symcat(A,B,C):-concat_atom([A,'_',B],C).

namecat(A,B,C,D):-concat_atom([A,B,C],D).

translate_one_file(Fname,Mode,Printer):-
  ( term_of(Fname,compiling,C),
      compile_clause(C,Mode,Printer),
    fail
  ; true
  ).

% add_instr(1,Op,Reg,F,N):-Is=[Op,Reg,F,N],maplist(type_of,Is,Ts),traceln(add_instr(Is,Ts)),fail.
add_instr(1,Op,Reg,F,N):-
  current_output(Stream),
  fwriteln(Stream,Op),
  fwriteln(Stream,Reg),
  fwriteln(Stream,N),
  fwriteln(Stream,F).

/******/

lco_mt:-time(concurrent_bootstrap).

concurrent_bootstrap:-
  findall(F,(included_file_of('../psrc/top.pl',F0),atom_concat('../psrc/',F0,F)),Fs),
  compile_mt(Fs,'../bin/lwam.bp').

%% compile_mt(Files,OutFile): compiles Files+built-ins concurrently and merges the results to a .bp file
compile_mt(Files,OutFile):-
  crossref_init, % see crossref_warnings in xtop.pl
  compile_mt(Files,true,true,OutFile),
  crossref_warnings.

%% wcompile_mt(Files,OutFile): compiles Files concurrently and merges the results to OutFile - a .wam file
wcompile_mt(Files,OutFile):-
  compile_mt(Files,false,true,OutFile).
  
compile_mt(Files,WithBuiltins,WithPost,OutFile):-
  BT='builtins.tmp',PT='post.tmp',
  maplist(make_wcompile_cmd,Files,XGs),
  
  multi_all(XGs,OutFiles),

  % traceln(here(XGs)),fail,
  
  (WithBuiltins->compile_builtins_to(BT),Os1=[BT|OutFiles];Os1=OutFiles),
  (WithPost->post_compile_to(PT),Os2=[PT|Os1];Os2=Os1),
  concatenate_files(Os2,OutFile),
  foreach(member(OF,Os2),delete_file(OF)).

% :- assert(hello(bye)). % this would be asserted when Prolog starts if used here
% however write statements would brake the generated code  !!!
  
make_wcompile_cmd(File,(OutFile:-
  to_wam_suffix(File,InFile,OutFile),
  WithBuiltins=false,WithPost=true, % if true avoids WithPost for dynamics
  compile0(InFile,OutFile,WithBuiltins,WithPost,'$file'))).

/*******/

% used in mt mode
compile_builtins_to(OutFile):-
 open(OutFile,write,OutStream),
 Mode=wam,
 current_output(S0),
 set_output(OutStream),
 cc_bbuiltins(Mode),
 close(OutStream),
 set_output(S0).

% used in mt mode
post_compile_to(OutFile):-
 open(OutFile,write,OutStream),
 current_output(S0),
 set_output(OutStream),
 post_compile(false,false,'$file'),
 close(OutStream),
 set_output(S0).
   
compile0(InFile,OutFile,WithBuiltins,WithPost,ToMemOrFile):- % ToMemOrFile=$mem or $file
  Printer=ttyprint,
  Mode=wam,
  current_output(S0),
  ( is_interactor(OutFile)->OutStream=OutFile
  ; open(OutFile,write,OutStream)
  ),
  set_output(OutStream),
  (WithBuiltins=true->cc_bbuiltins(Mode);true),
  translate_one_file(InFile,Mode,Printer),
  post_compile(WithBuiltins,WithPost,ToMemOrFile),
  close(OutStream),
  set_output(S0).

% note that dynamic stuff is executed atomically after everything has been
% read and when all compiled code is in place - just in case dynamic code
% calls it. This is different from "do it right away" when reading - e.g. SWI-Prolog


post_compile(_WithBuiltins,WithPost,ToMemOrFile):-
  WithPost==true,
  !,
  Clause=('$$_do_after_load'(H):-true), % to be used as template
  % to memory, but builtins already there
  % to .bp file - given that builtins have been also added
  ( 
    ToMemOrFile=='$file',
    db_clause('$$',to_wcompile(H0),true),
    undo_compile_side_effect(H0,H),
    compile_clause(Clause,wam,ttyprint),
    fail
  ;
    db_clause('$$',to_wcompile(H),true),
    % avoid_things_done_while_reading(ToMemOrFile,H), % fails if done in this process
    compile_clause(Clause,wam,ttyprint),
    fail
  ; db_clause('$$',to_wcompile(H),true),
    % leave no trace when compiling to .bp file
    % or, make sure preexisting dynamics are abolished when '$mem'
    undo_compile_side_effect(H,Action),
    call(Action), 
    fail
  ; db_abolish('$$',to_wcompile/1),
    fail
  ; db_clause('$$',initialization(X),true),
    H=initialization(X),
    compile_clause(Clause,wam,ttyprint),
    fail
  ; db_abolish('$$',('initialization')/1)
  ).
post_compile(WithBuiltins,WithPost,ToMemOrFile):-
   WithPost==false,WithBuiltins==false,ToMemOrFile=='$file',
   !,
  % to .wam file - dynamics lost as $$_do_after_load would be multifile 
  % NOT ANYMORE - loader will generate $$_do_after_load_0,1,... names
  ( db_clause('$$',C,true),
    traceln('!!! wcompile/2: UNIMPLEMENTED dynamics - missed action:'),
    traceln(C),
    fail
  ; db_clear('$$')
  ).  
post_compile(WithBuiltins,WithPost,ToMemOrFile):-
  traceln('BAD_FLAGS_IN_POST_COMPILE'(WithBuiltins,WithPost,ToMemOrFile)),
  errmes(bad_flags,post_compile(WithBuiltins,WithPost,ToMemOrFile)).
    

avoid_things_done_while_reading('$mem',dynamic(_FN)):-!,fail.
avoid_things_done_while_reading('$mem',index(_Pred)):-!,fail.
avoid_things_done_while_reading('$mem',multifile(_FN)):-!,fail.
avoid_things_done_while_reading('$mem',discontiguous(_FN)):-!,fail.
avoid_things_done_while_reading(_,_). % need to be done
  
undo_compile_side_effect(dynamic(FN),abolish(FN)).
undo_compile_side_effect(index(Pred),abolish(F/N)):-functor(Pred,F,N).
undo_compile_side_effect(multifile(FN),abolish(FN)).
undo_compile_side_effect(discontiguous(FN),abolish(FN)).

  
[File]:-compile(File).

%% [File] or compile(File): compiles in new name space File.pl (or File.wam, if newer)
compile(F0):- find_prolog_file(F0,F),push_cmd_arg(mcompile(F)),return(restart(F)).



%% qcompile(File) : when used from Java cmd line acts like if compile(File) was typed at '?-' prompt
qcompile(F0):-
  find_prolog_file(F0,F),
  % find_file(F0,F), %triggers bug - cannot find command line *.wam file
  mcompile(F).  

% mcompile(File) compiles to memory or loads wam file precompiled explicitely with wcompile  
mcompile(File0):-
  %traceln(mcompile(File0)),
  trim_prolog_file(File0,File,Suf),
  atom_concat(File,'.wam',WamFile),
  atom_concat(File,Suf, PlFile),
  (newer_file_of(WamFile,PlFile,F)->
    ( F=PlFile->Cmd=mcompile0(PlFile)
    ; Cmd=mload0(WamFile)
    )
  ; exists_file(WamFile)->Cmd=mload0(WamFile)
  ; Cmd=mcompile0(PlFile)
  ),
  %traceln(cmd(Cmd)),
  call(Cmd).

% ocompile(InFile):-
  
mcompile0(InFile):-
  exists_file(InFile),
  crossref_init, % see crossref_warnings in xtop.pl
  !,
  open('$mem',write,MFile),
  %traceln(here=MFile), 
  WithBuiltins=false,
  WithPost=true, % semantics is transactional - actions only happen at end
  % also, whem recompiling over an existing db we clean relevant dynamics one by one
  % before the asserts
  time(compile0(InFile,MFile,WithBuiltins,WithPost,'$mem'),T),
  println(compiled(InFile,time(T))),
  open(MFile,read,Interactor),
  jcall('vm.extensions.LineStreamInput',getReader(Interactor),Reader),
  external_top(InFile,Reader).
mcompile0(InFile):-
  errmes(file_not_found,InFile).
  
mload0(WamFileName):-
  wam_reader(WamFileName,Reader),
  external_top(WamFileName,Reader).

wam_reader(WamFileName,Reader):-
  open(WamFileName,read,Interactor),
  jcall('vm.extensions.LineStreamInput',getReader(Interactor),Reader).
  
crossref_init:-
  crossref_clear,
  index('$crossref'(1,0,1,0)),
  index('$crossref'(1,0)),
  % index('$crossref'(1,1)), % maybe needed in the future
  true.


% crossref_clear:-crossref_show,fail.  
crossref_clear:-abolish('$crossref'/4),abolish('$crossref'/2).


crossref_def(F/N):-clause('$crossref'(F,N),true),!.
crossref_def(F/N):-assert('$crossref'(F,N)).

crossref_undef(F/N):-clause('$crossref'(F,N),true),!,fail.
crossref_undef(_).

crossref_collect(F/N,T):-functor(T,FX,NX),assert('$crossref'(F,N,FX,NX)).

crossref_show:-stdio(_O),
    clause('$crossref'(F,N,G,M),true),
    traceln('*** crossref'((F/N:-G/M))),
  fail.
crossref_show:-stdio(_O),
    clause('$crossref'(F,N),true),
    traceln('*** crossref_def'(F/N)),
  fail.  
crossref_show.

%% crossref_warnings: prints crossref warning when compiling to memory 
crossref_warnings:-
  clause('$crossref'(F,N,G,M),true),
  crossref_undef(G/M),
  functor(Ref,G,M),
  crossref_check(Ref,F/N,G/M),
  fail.
crossref_warnings:-
  crossref_clear.

crossref_check(Ref,_,_):-is_compiled(Ref),!.  
crossref_check(Ref,_,_):-is_dynamic(Ref),!.
% crossref_check(_,_,G/M):-clause('$crossref'(G,M),true),!.
crossref_check(_,FN,GM):-traceln('*** WARNING: undefined in'(predicate(FN),reference(GM))).




%%  compile(InFile,OutFile): compiles including builtins and dynamics - it should include psrc/top.pl
compile(InFile,OutFile):-
  WithBuiltins=true,
  compile(InFile,OutFile,WithBuiltins).

compile(InFile,OutFile,WithBuiltins):- 
  crossref_init, % see crossref_warnings in xtop.pl
  WithPost=true,
  time(compile0(InFile,OutFile,WithBuiltins,WithPost,'$file'),T),
  println(compiled(InFile,to(OutFile),time(T))),
  crossref_warnings.

%% fcompile(FName): compiles FName.pl to FName.wam - including dynamics

fcompile(FName):-
  atom_concat(FName,'.pl',InFile),
  atom_concat(FName,'.wam',OutFile),
  compile(InFile,OutFile,false).

%% bcompile(F): compiles F.pl to F.bp - including builtins and dynamics - it should include psrc/top.pl
bcompile(FName):-
  atom_concat(FName,'.pl',InFile),
  atom_concat(FName,'.bp',OutFile),
  compile(InFile,OutFile,true).

%% wcompile(File): compiles File.pl to File.wam - can be loaded directly with qcompile(File) - no dynamics
wcompile(File0):-wcompile0(File0,_InFile,_Outfile).

wcompile0(File0,InFile,OutFile):-
  to_wam_suffix(File0,InFile,OutFile),
  wcompile(InFile,OutFile).

to_wam_suffix(File0,InFile,OutFile):-
  trim_prolog_file(File0,File,Suf),
  atom_concat(File,Suf,InFile),
  atom_concat(File,'.wam',OutFile).

trim_prolog_file(File0,File,Suf):-member(Suf,['.pl','.pro']),atom_concat(F,Suf,File0),!,File=F.
trim_prolog_file(File0,File,'.pl'):-member(Suf,['.wam']),atom_concat(F,Suf,File0),!,File=F.
trim_prolog_file(File,File,'.pl').

% semantics is now the same as fcompile as WithPost=true
wcompile(InFile,OutFile):-
  crossref_init,
  WithBuiltins=false,WithPost=false,ToMemOrFile='$file',
  compile0(InFile,OutFile,WithBuiltins,WithPost,ToMemOrFile),
  crossref_warnings.

%% lco: bootstrapping - self compiles by compiling ../psr/top.pl to ../bin/wam.bp  
lco:-compile('../psrc/top.pl','../bin/lwam.bp').


  
  
 % end    