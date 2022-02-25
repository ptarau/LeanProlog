java_only_test:-btest,fail.
java_only_test:-xtest,fail.
java_only_test:-itest,fail.
java_only_test:-symtest,fail.
java_only_test:-stest,fail.
% java_only_test:-toploop,halt(0).

btest:-this_class(X),traceln(this_class____________(X)),fail.
btest:-current_engine(X),traceln(current_engine____________(X)),fail.
btest:-new_engine(X,member(X,[a1,a2,a3]),E),
  engine_get(E,A),
  engine_get(E,B),
  traceln(new_engines________(E=[A,B])),
  fact(20,FA),
  load_engine(E,Xs,fperm(FA,30,Xs)),
  engine_get(E,Xs),
  engine_get(E,Ys),
  traceln(load_engines_______([E=Xs,E=Ys])),fail. 
btest:-findall(Xs,perm([1,2,3],Xs),Xss),traceln(findall_____(Xss)),fail.

btest:-T=f(a,A,A,b),arg(0,T,F),arg(1,T,A),arg(2,T,AA),traceln(arg_____(T,[F,A,AA])),fail.
btest:-T=f(a,A,A,b),arity(T,N),traceln(arity_____(T,N)),fail.
btest:-fun(f,4,T),arity(T,N),arg(0,T,F),arg(1,T,A),arg(2,T,B),traceln(fun_____(T,F,N,A,B)),fail.
btest:-fun(f,3,T),fun(T,6,TT),traceln(fun__fun_____(T,TT)),fail.
btest:-succ(99,A),succ(A,B),traceln(succ_____(A,B)),fail.
btest:-call(perm([1,2,3]),R),traceln(call2____(R)),fail.
btest:-call(append([1,2,3]),[4,5],R),traceln(call3____(R)),fail.
btest:-cnl,ereverse([1,2,3],R),traceln(ereverse____(R)),fail.
btest:-cnl,freverse([1,2,3],R),traceln(freverse____(R)),fail.
btest:-cnl,between(1,4,R),traceln(between____(R)),fail.
btest:-rtest,traceln(after_retest),fail.
btest:-X=funny,
   catch(
      if(X=funny,throw(is_funny(X)),traceln(boring)),
      is_funny(Res),
      traceln(Res)
   ),fail.
btest:-T=f(a,b,X,X),functor(T,F,N),functor(TT,F,N),traceln(functor_____(T,TT)),fail.
btest:-T=f(a,X,X,b),T=..FXs,TT=..FXs,traceln(univ_____(T,FXs,TT)),fail.
btest:-T=f(a,[X,Y,10],g(X,3.14),h(X,b,Y)),
       copy_term(T,TT),
       traceln(copy_term_____(T)),
       traceln(copy_term_____(TT)),fail.
btest:-type_of(_,V),type_of(99,I),type_of(a,C),type_of(a(1),F),traceln(type_of_____(V,I,C,F)),fail.

btest:-succ(_,X),traceln(void____(X)),fail. % errmes - ok
btest:-traceln(finished_btest),fail.

fperm(X,Y,Xs):-fact(Y,F),perm([X,F,X],Xs).

xtest:- -(30,R),traceln(xtest_minus___(R)),fail.
xtest:- -('300000000000000',R),traceln(xtest_long_minus___(R)),fail.

xtest:- +(10,20,R),traceln(xtest_plus___(R)),fail.
xtest:- +('100000000000000','200000000000000',R),traceln(xtest_long_plus___(R)),fail.
% it looks like this bug is with the reader in the compiler ...
xtest:- +('1000000000000000000000','2000000000000000000000',R),traceln('BUG_xtest_big_plus___'(R)),fail.

xtest:- -(30,20,R),traceln(xtest_sub___(R)),fail.
xtest:- -('300000000000000','200000000000000',R),traceln(xtest_long_sub___(R)),fail.

xtest:-fact(5,R),traceln(xtest_fact___(R)),fail.
xtest:-fact(30,R),traceln(xtest_big_fact___(R)),fail. % =>265252859812191058636308480000000

xtest:- +(3.14,10,R),traceln('BIG_DECIMAL______________'(R)),fail.
xtest:-fact(5.0001,R),traceln(xtest_big_fact___(R)),fail. % =>265252859812191058636308480000000

% xtest:-cnl,traceln('TERMS:'),cnl,ttest,cnl,fail.

xtest:-cnl,traceln('GCTEST STARTS'),gctest,cnl,traceln('GCTEST ENDED'),cnl,fail.

xtest:-traceln(finished_xtest).

% memory overflow test

looptest:-loop(22).

loop(Max):- <<(Max,1,N),loop(N,0).

loop(0,_):-write(done),nl.
loop(N,X):-N>0,-(N,1,N1),loop(N1,c(X,N)).

memover:-mtest([0,1,2],_Ps).

mtest(Xs,Ps):-mtest(Xs,Xs,Ps).

mtest([],Ps,Ps).
mtest([_|Ns],Xs,Ps):-all_permutations(Xs,Ys),mtest(Ns,Ys,Ps).

% large numbers etc.

fact(N,X):-N<2,!,X=1.
fact(N,R):- -(N,1,N1),fact(N1,X1),*(X1,N,R). %,traceln(times(X1,N,R)).  

gctest:-
  numloop(0,40),
  fail.
gctest:-
  fact(50,N),
  pow(2,75,M),
  gc,
  /(N,M,P),
  traceln(divided=P),
  '//'(N,M,R),
  traceln(div=R),
  gc,
  '<<'(M,3,Z),
  '>>'(M,3,U),
  traceln(shift(Z,U)),
  X=14,
  bitcount(X,BC),
  traceln(bitcount(X,BC)),
  pow(2.0001,3,PW),
  traceln(pow(PW)),
  gcd(M,N,D),
  traceln(gcd(D)),
  Codes="hellooooooooooooo",
  new_fun1('$string',Codes,FS),
  traceln(here=FS), % G_STRING
  from_codes(Codes,S),
  type_of(S,Type),
  traceln(atom(Type,S)),
  to_codes(S,NewCodes),
  traceln(codes(NewCodes)),
  fact(25,FX),type_of(FX,TX),
  traceln(type(TX,FX)),
  fact(5,FY),type_of(FY,TY),
  traceln(type(TY,FY)),
  to_ncodes(FX,SX),
  traceln(to_ncodes(number,FX,SX)),
  % $$$
  to_ncodes(FY,SY), %120
  traceln(to_ncodes(number,FY,SY)),
  from_ncodes(SX,FX1),type_of(FX1,TX1),functor(FX1,Fun1,_),
  traceln(from_ncodes(type(TX1,Fun1),FX1)),
  if(number(FX1),traceln(number),traceln(should_be_number(FX1))),
  from_ncodes(SY,FY1),type_of(FY1,TY1),
  traceln(from_codes(type(TY1),number,FY1)),
  if(number(FY1),traceln(number),traceln(should_be_number(FY1))),
  fail.
 
numloop(N,M):-N>M,!.
numloop(N,M):- +(N,1,N1),
  maybegc1(N),
  maybegc2(N),
  numloop(N1,M).

maybegc0(N):- member(N,[2,5,7,10,19,25,27]),!,gc.
maybegc0(_).

maybegc1(N):- *(N,N,N2),fact(N,F),maybegc0(N),-(F,N2,R),traceln(N+R).

maybegc2(N):-N>100,N<200,!,
  new_engine(N,maybegc1(N),E),engine_get(E,_),
  maybegc3(N,E).
maybegc2(N):-N>10,N<20,!,
  new_engine(N,maybegc1(N),E),engine_get(E,_),
  maybegc3(N,E).
maybegc2(_).

maybegc3(N,E):-N>15,N<20,!,stop(E).
maybegc3(N,E):-N>105,N<120,!,stop(E).
maybegc3(_N,_E).

itest:-traceln('STARING itest'),fail.
itest:-write('           HELLO from ConsoleIO'),nl,fail.
%itest:-readln(T),type_of(T,TT),write(got(TT,T)),nl,fail.
itest:-parsertest,fail.
itest:-pow(2,3,X),traceln(pow=X),fail.
itest:-jcall('vm.logic.Interact',warnmes('static Java method called with reflection'),_),fail.
% itest:-toploop,fail.
itest:-traceln('FINISHED itest').

symtest:-traceln('STARTING symtest'),fail.
symtest:-
  between(1,1000,I),
    number_codes(I,Cs),atom_codes(S,Cs),mod(I,100,0),
    println(S),
  fail.
  
%symtest:-traceln('FOLDALL:'),foldall((+),(X^member(X,[1,2,3,4])),R),println(foldall(R)),fail.
symtest:-traceln('FINISHED symtest').

% db tests

atest:-
  assert(a(1)),
  assert(a(2)),
  assert(a(3)),
  assert(b(2)),
  assert(b(3)),
  assert(b(4)),
  assert((c(X):-a(X),b(X))),
  assert(c(5)),
  listing,
  c(X),println(X),
  fail.

dbgo:-
  new_eb(Db),
  eb_assertz(Db,(a(2):-true)),
  eb_asserta(Db,(a(1):-true)),
  eb_assertz(Db,(b(X):-a(X))),
  ( eb_clause(Db,H,B),
    println((H:-B)),
    fail
  ; true
  ),
  stop(Db).
 
stest:-
  new_state(Db),
  set_state_of(Db,a,f(X,
                g(X,3.14)
               )
            ),
  get_state_of(Db,a,A),
  println(a-A),
  ( between(0,10,I),
    set_state_of(Db,I,f(I,I)),
    fail
  ; true
  ),
  ( all_state_of(Db,H,B),
    println(H-B),
    fail
  ; true
  ),
  stop(Db).
   
 
% yield tests

yloop(X):-engine_yield(X=>Y),yloop(Y).

inc(X1,X2):-succ(X1,X2).

inc_test([R1,R2]):-
   new_engine(_,yloop(0),E),
   ask_engine(E,(X=>Y:-inc(X,Y)),R1),
   ask_engine(E,(X=>Y:-inc(X,Y)),R2).

fdb:-
 (
    line_of('progs/simple.pro',L),
      println(L),
    fail
 ;  println('end'),nl
 ).
  
iotest:-
  open('progs/simple.pro',read,S),
  repeat,
    %readln(S,Line),
    get(S,A),
    ( 
       A=the(Line)->println(Line),fail
    ;  !
    ),
  close(S),
  true.

bttest:-
 ( f(X,g(X,Y))==f(X,g(X,Y)),
  f(X,h(Y))\==f(Y,h(X)),
  !,
  println(ok)
; printlen(problem)
).

bttest1:-
  X is 2+3,
  X<2+8.


  
% ----------------------------
% emulates usual I/O
xinit_code:-
  new_engine(C,member(C,"f(g(X,X,Y,1,abc),h(1.23,Y))."),E),
  set_state(reader,E).

 
xget_code(C):-
  get_state(reader,E),
  get(E,X),
  (X=the(A)->C=A;C is -1).
     
% tests

data(
"f(X,s(X))."
).
data(
"f:-g,h."
).
/* % bug
data(
"f(X,s(X)):-
   a(Y1,13,2,  Y1 ),!,
   g(X,b).").
*/   
data(
"a([X|Xs],Ys,[X|Zs]):-a(Xs,Ys,Zs).").

data(
"go(Xs,Ys):-a([1,2,3],[4,5|Xs],Ys)."
).

data(
"a(X):- (a,b(X),c), d(X)."
).

data(
"b(X):- ((a(X);b(X));c(X)), d(X),e(X)."
).

data(
"c(X):- ( a(X) -> b(X) ; c(X) )."
).

/* % bug
data(
"d(X):- 
   ( a(X), b(X) -> c,d,e ; c(X)->d ;  f(X),g,((h)) 
   ).").
*/

data("d((H:-B)):-a(H),d((B->t;f)),show(A,B,(A:-B)).").

data("a:-println(""bye"").").

data("a(X):-X=:=x.").

data("a(X):- /* hello */ b(X).").

data("bad('+').").

data("applyF('+',_,X1,X2,X3):-X3 is \/(X1,X2).").

data("a(3.14).").

data("+(1,2,3).").

/*
toks:-
  init_code,
  read_tokens(Tokens, Variables),
  traceln(tokens(Tokens, Variables)).
*/
  
toktest:-data(Cs0),
  append(Cs0," ",Cs),
  write_codes(Cs),
  ( tokenizer(Cs,Ws)->write(Ws),nl,write('!!!yes')
  ; write('no!!!')
  ), nl,nl,fail.

% write_codes(Cs):-atom_codes(A,Cs),write(A),nl.

wstest:-tokwords(Ws,"X is /* ok */ 1.",[]),println(Ws).
wstest1:-Xs="X is 1. % ok! ",append(Xs,[10],Is),words(Ws,Is,[]),println(Ws).

% sptest:-space("  /* ok */   ",[]).

parsertest:-
   data(Cs0),append(Cs0," ",Cs),
   write_codes(Cs),
   (codes2clause(Cs,T,Vs)->
     write(T+Vs),nl,
     M='yes!!!'
     ;
     M='no!!!'
   ),
   write(M),nl,nl,
   fail.

fltest:-
(
  line_of('progs/bits.pro',T),
  println(T),
  fail
; println(done)
).

fctest:-
  codes_of('progs/bits.pro',Cs),
  atom_codes(A,Cs),
  println(A).
  
ftoks:-
 ( tokens_of('progs/bits.pro',Cs),
  member(C,Cs),
  println(C),
  (C=eoc->nl;true),
  fail
; println(done)
).

fltoks:-
 ( token_lists_of('progs/dsyn.pro',Tss),
  member(Ts,Tss),
  println(Ts),
  fail
; println(done)
).

sfterms:-
 ( term_of('progs/simple.pro',C),
  portray_clause(C),
  fail
; println(done)
).

fterms:-
 ( term_of('progs/bits.pro',C),
  portray_clause(C),
  fail
; term_of('progs/dsyn.pro',C),
  portray_clause(C),
  fail
; portray_clause(done)
).      

% pp(X):-portray_clause(X).

xterms0:-
(  all_terms_of('../psrc/top.pl',Cs),pp(Cs),
  member(C,Cs),
  is_fterm(C),
  pp(C),
  fail
; nl
).

/*
xterms:-
  terms_of('../psrc/top.pl',Cs,Fs,Ps), % ??
  maplist(pp,Cs),
  maplist(pp,Fs),
  pp(Ps).
*/

/* % bug
xterm:-
 ( term_of('../psrc/top.pl',C,Vs),
  pp(C),
  pp(Vs),
  nl,
  fail
; nl
).
*/

nobug:-
 parser([const(bad),lpar,const('+'),rpar,eoc],_T,_Vs).


w(Xs,Xs):-println(here(Xs)).
w1:-traceln('HERE !!!').
w2:-traceln('THERE !!').
ww(X):-traceln('*** TRACE'(X)).

com1:-
  T1=f(a,b),
  T2=f(a,b),
  compare0(T1,T2,R),
  write(T1),write(R),write(T2),nl.

com2:-
  T1=f(3,5),
  T2=f(5,3),
  compare0(T1,T2,R),
  write(T1),write(R),write(T2),nl.
  
com3:-
  T1=f(X,Y),
  T2=f(Y,X),
  compare0(T1,T2,R),
  write(T1),write(R),write(T2),nl.
 
 com4:-
   sort([2,a,4,_X,_Y,f(b),f(c),f(g(a))],R),
   println(R).
   
ktest:-
 ( keygroup([1-a,3-b,2-b,2-a],R,C),
   println(R+C),
   fail
; nl
).  

xco:-
  (
    term_of('../psrc/top.pl',C,Vs),
    println(C),
    println(Vs),
    nl,
    fail
  ; nl
  ).
        
lpterms:-
  open('lpterms.txt',write,O),
  ( term_of('../psrc/top.pl',T),
    % write(O,T),nl(O),
    to_functor(T,FN),write(O,FN),nl(O),
    fail
  ; true
  ),
  close(O).
    
to_functor((H:-_),F):-!,functor(H,F,_N).
to_functor(H,F):-functor(H,F,_N).

must_succeed(G):-G,!.
must_succeed(G):-ttyprint('UNEXPECTED FAILURE IN:'(G)),fail.

zzz(G):-must_succeed(G).

mtest1:-
  open('$mem',write,M),
  println(M,f(A,A)),
  println(M,g(3.14)),
  close(M),
  traceln(inmem(M)).
 
mc1:-
  open('$mem',write,M),
  println(M,f(A,A)),
  println(M,g(3.14)),
  close(M),
  open(M,read,F),
  readln(F,T1),println(T1),
  readln(F,T2),println(T2),
  readln(F,TX),println(TX),
  close(F).

mci:-compile('progs/pISO.pro').

mcs:-compile('progs/simplest.pro').

mcx:-compile('progs/dsyn.pro').

 
ttest1:-
  foreach(
    term_of( '../psrc/top.pl',_),
    fail
  ).

ttest1a:-
  foreach(
    term_of( '../psrc/top.pl',T),
    assert(T)
  ).

ttest2:-
  open('test.pro',write,F),
  foreach(
    term_of( '../psrc/top.pl',T),
    println(F,T)
  ),
  close(F).

ttest3:-  
  foreach(
    tokens_of( 'test.pro',_X),
    fail
  ).

ttest4:-  
  foreach(
    line_of( 'test.pro',Xs),
    traceln(Xs)
  ).

ttest5:-  
  foreach(
    code_of( 'test.pro',_X),
    fail
  ).  

ttest6:-  
  time(token_lists_of('test.pro',_)).
       
       
ttest7:-
  foreach(
    term_of('../psrc/jtests.pl',T),
    println(T)
  ).
 
catest:-
  catch(cagoal(X),Ex,println(caught(Ex))),
  println(got(X)),
  fail.
  
  
cagoal(X):-member(X,[1,2,3,4]),(X=3->throw(exception(X)),fail;true).
        
        
oversym1(N):-
  for(I,1,N),
  atom_concat(s,I,_),
  fail.
oversym1(_).


symtime:-cputime(T1),symgoal(4,100000),cputime(T2),T is T2-T1,statistics,println(time=T),halt.

symgoal(I,N):-addoversyms(I,N,TG),foreach(run_tasks(TG,R),println(R)).
    
 
addoversyms(0,_,_).
addoversyms(I,N,TG):-I>0,I1 is I-1,add_task(Info,oversym(I,N,_,Info),TG),addoversyms(I1,N,TG).
    
oversym(I,N,[R|Rs],Info):-
  N>0,
  !,
  N1 is N-1,
  atom_concat(s,N,SN),
  atom_concat(SN,I,R),
  % println(R),
  oversym(I,N1,Rs,Info).
oversym(_,0,[],info(S,E)):-symbols(S),engines(E).
    
% end
  