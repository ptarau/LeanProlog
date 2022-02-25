/*
File compiler interface. Shared between BP and SWI based compilers.
*/

fcompile(Project,Target):-
  fcompile(wam,[Project],Target,ttyprint).
  
fcompile(Mode,Fs,OutFile,Printer):-
  call(Printer,compile_to(Mode,Fs)),
  ctime(T1),
  xcompile(Mode,Fs,OutFile,Printer),
  ctime(T2),
  T is T2-T1,
  call(Printer,total_compile_time(T)).

xcompile(Mode,InFiles,OutFile,Printer):-
  telling(CF),
  tell(OutFile),
  call(Printer,begin_generating(builtins)),
  cc_bbuiltins(Mode),
  call(Printer,end_generating(builtins)),
  jcompile(InFiles,Mode,Printer),
  told,
  tell(CF).

wcompile(File):-
  atom_concat(File,'.pl',InFile),
  atom_concat(File,'.wam',OutFile), 
  telling(CF),
  tell(OutFile),
  jcompile([InFile],wam,ttyprint),
  told,
  tell(CF).
    
jcompile(Files,Mode,Printer):-
  translate_all_files(Files,Mode,Printer).

translate(end_of_file,_,_):-!.
translate(':-'([F|Fs]),Mode,Printer):-!,
  include_file([F|Fs],Mode,Printer),
  fail.
translate('::-'(H,B),Mode,_):-compile_bin(Mode,(H:-B)),fail.
translate(C,Mode,_):-cc(Mode,C),fail.

include_file(Fs,Mode,Printer):-
  call(Printer,begin_including(Fs)),
  translate_all_files(Fs,Mode,Printer),
  call(Printer,end_including(Fs)).

translate_all_files(Files,Mode,Printer):-
  nonvar(Files),Files=[_|_],
  !,
  (
    member(F,Files),
      translate_one_file(F,Mode,Printer),
    fail
  ; true
  ).
translate_all_files(Fname,Mode,Printer):-
  translate_one_file(Fname,Mode,Printer).
  

% end
