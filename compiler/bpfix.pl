% gets rid of some unused operators

:-op(0,fx,(dynamic)).
:-op(0,fx,(mode)).
:-op(0,fx,(module)).
:-op(0,fx,(public)).
:-op(0,fx,(memo)).
:-op(0,fx,(type)).
:-op(0,fx,(delphi)).
:-op(0,fx,(mod)).
:-op(0,fx,(extends)).
:-op(0,fx,(with)).

:-op(200,fx,(`)).
:-op(200,fx,(#)).
:-op(200,xfx,(:)).
:-op(600,xfx,(~>)).
:-op(700,xfx,(=>)).
:-op(700,xfx,(<=)).
:-op(700,xfx,(==>)).
:-op(700,xfx,(<==)).
:-op(900,xfx,(*->)).

% add whatever is missing

xboot:-
  cd('../psrc'),
  lco('top.pl','../bin/lwam.bp'). 
 
lboot:-
  cd('../psrc'),
  fcompile('top.pl','../bin/lwam.bp').
  
translate_one_file(Fname,Mode,Printer):-
  find_file(Fname,F),
  seeing(F0),see(F),
  repeat,
    read_clause(C),
    translate(C,Mode,Printer),
  !,
  seen,see(F0),
  translate_delayed.

csep:-cnl.

add_instr(1,Op,Reg,F,N):-
  cwrite(Op),csep,
  cwrite(Reg),csep,
  cwrite(N),csep,
  cwrite(F),cnl.
 
translate_term_of(Fname,Mode,Printer):-
  ( term_of(Fname,T),
      T=C, %expand_term(T,C),
      translate(C,Mode,Printer),
    fail
  ; true
  ),
  translate_delayed.

lco(InFile,OutFile):- 
  ttyprint(compiling(InFile,to(OutFile))),
  Printer=ttyprint,
  Mode=wam,
  tell(OutFile),
  cc_bbuiltins(Mode),
  translate_term_of(InFile,Mode,Printer),
  told.
 
concat_atom(As,A):-map(name,As,Css),appendN(Css,Cs),name(A,Cs). 
  
bpterms:-
  tell('bpterms.txt'),
  cd('../psrc'),
  ( term_of('../psrc/top.pl',T0),
    expand_term(T0,T),
    % println(T),
    to_functor(T,FN),println(FN),
    fail
  ; true
  ),
  told.

to_functor((H:-_),F):-!,functor(H,F,_N).
to_functor(H,F):-functor(H,F,_N).
  
  
atom_concat(X,Y,Z):-namecat('',X,Y,Z).

  