% parser.pl

toks2term(Toks,Term):-topTerm(R,Toks,[]),!,Term=R.
toks2term(Toks,syntax_error(Toks)).

prefix(OP, P,XP ):- get_pri(OP,fx,P),XP is P-1.
prefix(OP, P,P ):- get_pri(OP,fy,P). 

infix(OP,P, XP,XP):-get_pri(OP,xfx,P),XP is P-1.
infix(OP,P, XP, P):- get_pri(OP,xfy,P),XP is P-1. 
infix(OP,P,  P,XP ):- get_pri(OP,yfx,P),XP is P-1.

postfix(OP, P,XP ):- get_pri(OP,xf,P),XP is P-1.
postfix(OP, P,P ):- get_pri(OP,yf,P).

topTerm(T)-->lTerm(T,1200).
commaTerm(T)-->lTerm(T,999).

lTerm(T, PP) --> [num(X)],!, {to_number(X,N)},rTerm(N,T,0,PP). 
lTerm(T, PP) --> [var(V)],!, rTerm(V,T,0,PP). 
lTerm(T, PP) --> [str(X)],!, {atom_codes(X,Cs)},rTerm(Cs,T,0,PP).

lTerm(T, PP) --> lpar(Par),!,parTerm(Par,TI),rTerm(TI,T,0,PP). 

lTerm(T, PP) --> ['-',num(X)],!, {to_number(X,N1),-(0,N1,N)},rTerm(N,T,0,PP). 
lTerm(T, PP) --> ['+',num(X)],!, {to_number(X,N)},rTerm(N,T,0,PP). 
lTerm(T, PP) --> [OP], noStickyPar,{prefix(OP,P, RAP),P=<PP},!, 
  lTerm(Arg,RAP),
  {newTerm1(OP,Arg,NT)},
  rTerm(NT,T,P,PP).
lTerm(T, PP) --> [qtd(A)],!,funTerm(A,T,PP).
lTerm(T, PP) --> [A],{atom(A)},funTerm(A,T,PP).

parTerm(Par,Nil)-->rpar(Par),{par2nil(Par,Nil)}.
parTerm(Par,T)-->[NotOp],{atom(NotOp)},rpar(Par),{parprotector(Par)},!,{T=NotOp}.

parTerm(Par,List)-->{Par=list},!,  % x(list),
  listTerm(Par,List).

parTerm(Par,MaybeCurly)-->topTerm(TI),rpar(Par),{maybeCurly(Par,TI,MaybeCurly)}.

funTerm(F,T,PP)-->{Par=fun},lpar(Par),!,
  fTerm(Par,Xs),{FTI=..[F|Xs]},rTerm(FTI,T,0,PP). 
funTerm(A,T,PP)-->rTerm(A,T,0,PP). 

fTerm(Par,[T|Ts])-->commaTerm(T),fTerms(Par,Ts).

fTerms(Par,[])-->rpar(Par),!.
fTerms(Par,[X|Xs])-->[','],commaTerm(X),fTerms(Par,Xs).

% x(Mes,Xs,Xs):-traceln('!!!'(Mes,Xs)).

listTerm(Par,[])-->rpar(Par),!.
listTerm(Par,[X|Xs])-->[','],!, % x(comma),
  commaTerm(X),listTerm(Par,Xs). 
listTerm(Par,Xs)-->['|'],!, % x(bar),
  commaTerm(Xs),rpar(Par).
listTerm(Par,[X|Xs])--> % x(first),
  commaTerm(X),listTerm(Par,Xs).

rTerm(Term,T,LeftP,PP)--> [OP],{infix(OP,P, LAP,RAP), P=<PP,LeftP=<LAP},!, 
  lTerm(Arg2,RAP),
  {newTerm2(OP,Term,Arg2,NT)},
  rTerm(NT,T,P,PP). 
rTerm(Term,T,LeftP,PP)--> [OP],{postfix(OP,P,LAP), P=<PP,LeftP=<LAP},!,
  {newTerm1(OP,Term,NT)},
  rTerm(NT,T,P,PP).
rTerm(E,E,_,_)-->[]. 

maybeCurly(gram,T,'{}'(T)).
maybeCurly(op,T,T).
maybeCurly(fun,T,T).

parprotector(op).
parprotector(fun).

newTerm2(OP,Arg1,Arg2,NT):-NT=..[OP,Arg1,Arg2].
newTerm1(OP,Arg,      NT):-NT=..[OP,Arg].

par2nil(list,'[]').
par2nil(gram,'{}').

noStickyPar-->['('],!,{fail}.
noStickyPar-->[].

lpar(fun)-->['('].
lpar(op)-->['~('].
lpar(list)-->['['].
lpar(gram)-->['{'].

rpar(fun)-->[')'].
rpar(op)-->[')'].
rpar(list)-->[']'].
rpar(gram)-->['}'].

