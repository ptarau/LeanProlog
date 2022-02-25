%% to_canonical(T,CT): converts a term to parser independent representation - ## libs

to_canonical(T,[N|Gs]):-
  copy_term(T,CT),numbervars(CT,0,N),
  to_canonical(CT,Xs,[]),Gs=Xs.

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
from_canonical([_N|Cs],T):-
  init_dict(D),
  from_canonical(CT,D,_,Cs,[]),!,T=CT.

from_canonical(V,D1,D2)-->[v,T],{dict_ensure_added(T,V,D1,D2)}.
from_canonical(T,D,D)-->[s,T].
from_canonical(T,D,D)-->[n,T].
from_canonical(T,D1,D2)-->[f,N],
  from_canonicals(N,Xs,D1,D2),
  {T=..Xs}.

from_canonicals(0,[],D,D)-->!,[].
from_canonicals(K,[X|Xs],D1,D3)-->{K1 is K-1},
  from_canonical(X,D1,D2),
  from_canonicals(K1,Xs,D2,D3).


gg:-
  T1=fun(abc,goo(X,X,3.14,hi(Y,Y)),999),
  to_canonical(T1,Xs),
  write(Xs),nl,
  write(T1),nl,
  from_canonical(Xs,T2),
  write(T2),nl,
  fail.
  
dg:-
  init_dict(D1),
  dict_put(a,X,D1,D2),
  dict_put(b,Y,D2,D3),
  dict_get(a,XX,D3,D4),
  dict_get(b,YY,D4,D5),
  println(X+Y=XX+YY),
  println(D5).
  
init_dict(D):-hinit_eb(63,D).

dict_put(H,B,D,NewD):-
  h_server_step(is_dynamic(H),D,D1,63,yes),
  !,
  h_server_step(delete_element((H:-_)),D1,D2,63,yes),
  h_server_step(add_element((H:-B)),D2,NewD,63, yes).
dict_put(H,B,D,NewD):-
  h_server_step(push_element((H:-B)),D,NewD,63, yes).

dict_ensure_added(H,B,D1,D2):-dict_get(H,B,D1,D2),!.
dict_ensure_added(H,B,D1,D2):-dict_put(H,B,D1,D2).

dict_get(H,B,D,NewD):-
  h_server_step(queue(H),D,NewD,63,[(H:-B)]).

dict_remove(H,D,NewD):-
  h_server_step(is_dynamic(H),D,D1,63,yes),
  !,
  h_server_step(delete_element((H:-_)),D1,NewD,63,yes).
  
  
  