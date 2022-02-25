rterm_of(F,C):-
  find_file(F,File),
  ttyprint(opening(File)),
     term_of(File,C0),
     ( C0=':-'([_|_])->arg(1,C0,Fs),member(IF,Fs),rterm_of(IF,C)  
     ; C=C0
     ).

translate_rfile(Fname,Mode,Printer):-
  ( rterm_of(Fname,C),
      compile_clause(C,Mode,Printer),
    fail
  ; true
  ).
  
xadd_instr(1,Op,Reg,F,N):-
  telling(Stream),
  println(Stream,Op),
  println(Stream,Reg),
  println(Stream,N),
  println(Stream,F).

compile(InFile,OutFile):- 
  cputime(T1),
  Printer=ttyprint,
  Mode=wam,
  LinkFlag=0,
  tell(OutFile),
  cc_bbuiltins(Mode),
  translate_rfile(InFile,Mode,Printer),
  told,
  cputime(T2),
  T is T2-T1,
  println(time(compile(InFile,OutFile),T)).

lco:-compile('../psrc/top.pl','../bin/pwam.bp').
 
ttest0:-
  ctime(T1),
  foreach(
    rterm_of( '../psrc/top.pl',_),
    fail
  ),
  ctime(T2),
  T is T2-T1,
  println(time(T)).

ttest1:-
  foreach(
    rterm_of( '../psrc/top.pl',T),
    assert(T)
  ).

      
 % end    