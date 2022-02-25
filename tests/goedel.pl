%% to_canonical(T,CT): converts a term to parser independent representation - ## libs

to_canonical(T,[N|Gs]):-
  numbervars(T,0,N),
  to_canonical(T,Xs,[]),
  Gs=Xs.

to_canonical('$VAR'(T))-->!,[v,T].
to_canonical(T)-->{atom(T)},!,[s,T].
to_canonical(T)-->{number(T)},!,[n,T].
to_canonical(T)-->
  {compound(T),'=..'(T,Ts),length(Ts,L)},
  [f,L],
  to_canonicals(Ts).

  
/*
to_canonical('$VAR'(T))-->!,
   [v,L],
   {number_codes(T,Cs),length(Cs,L)},
   inject_terminals(Cs).
to_canonical(T)-->{atom(T),!,atom_codes(T,Cs),length(Cs,L)},
   [s,L],
   inject_terminals(Cs).
to_canonical(T)-->{number(T),!,number_codes(T,Cs),length(Cs,L)},
   [n,L],
   inject_terminals(Cs).
to_canonical(T)-->{compound(T),'=..'(T,Ts),length(Ts,L)},[f,L],to_canonicals(Ts).
  
inject_terminals([T|Ts])-->!,[T],inject_terminals(Ts).
inject_terminals([])-->[].
*/

to_canonicals([T|Ts])-->!,to_canonical(T),to_canonicals(Ts).
to_canonicals([])-->[].

%% from_canonical(CT,T): converts a term back to normal Prolog representation
from_canonical([_N|Cs],T):-from_canonical(CT,Cs,[]),!,T=CT.

from_canonical('$VAR'(T))-->[v,T].
from_canonical(T)-->[s,T].
from_canonical(T)-->[n,T].
from_canonical(T)-->[f,N],
  from_canonicals(N,Xs),
  {T=..Xs}.

from_canonicals(0,[])-->!,[].
from_canonicals(K,[X|Xs])-->{K1 is K-1},
  from_canonical(X),
  from_canonicals(K1,Xs).


gg:-
  T1=fun(abc,goo(X,X,3.14,hi(Y,Y)),999),
  to_canonical(T1,Xs),
  write(Xs),nl,
  write(T1),nl,
  from_canonical(Xs,T2),
  write(T2),nl,
  fail.
  

