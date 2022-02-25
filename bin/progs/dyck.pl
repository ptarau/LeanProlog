/*
  Determistic parser for generalized Dyck languages
  Handles arbitrary number of nested parentehses
  mixed with arbitrary non-parentehseis symbols.
  Note: par pairs are specified with leftPar+rightPar terminals
*/

go:-parse_pars(['{','[','{',1,2,3,'}','(',a,b,')',']','}'],T),write(T),nl,fail;true.

go1:-parse_pars(['{',a,'[',b,c,']','}'],T),write(T),nl,fail;true.

go2:-parse_pars(['{',a,b,'[',c,d,e,']','}'],T),write(T),nl,fail;true.

go3:-parse_pars(['{',a,b,'{',3.14,'(',c,d,'[',1,']',2,3,')',d,'}',e,'}'],T),write(T),nl,fail;true.

go4:-parse_pars([100,'{',a,b,'}','(',3.4,22,')',200],T),write(T),nl,fail;true.


%% parses a parenthesized expression to a term, fails if invalid input given
parse_pars(Xs,T):-lpts(T,Xs,[]).

lpt(parexp(Par,Es)) --> leftPar(Par),!,rpt(Par,Es).
lpt(E)-->nonpar(E).

lpts([E|Es])-->lpt(E),!,lpts(Es).
lpts([])-->[]. %nonpars(Es).

rpt(Par,[])-->rightPar(Par),!.
rpt(Par,Es)-->lpts(Es),rightPar(Par).

leftPar(round)-->['('].
leftPar(bracket)-->['['].
leftPar(curly)-->['{'].

rightPar(round)-->[')'].
rightPar(bracket)-->[']'].
rightPar(curly)-->['}'].

nonpar(_)-->leftPar(_),!,{fail}.
nonpar(_)-->rightPar(_),!,{fail}.
nonpar(X)-->[X].
