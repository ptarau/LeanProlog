%% to_canonical(T,CT): converts a term to parser independent representation - ## libs
to_canonical(T,CT):-atomic_to_canonical(T,X),!,CT=X.
to_canonical(T,CT):-compound_to_canonical(T,CT).

%% from_canonical(CT,T): converts a term back to normal Prolog representation - ## libs
from_canonical(CT,T):-canonical_to_atomic(CT,X),!,T=X.
from_canonical(CT,T):-from_compound_canonical(CT,T).

atomic_to_canonical('$VAR'(V),v(V)).
atomic_to_canonical(T,s(Cs)):-atom(T),atom_codes(T,Cs).
atomic_to_canonical(T,i(T)):-integer(T).
atomic_to_canonical(T,r(Cs)):-float(T),number_codes(T,Cs).

canonical_to_atomic(v(T),T).
canonical_to_atomic(s(Cs),T):-atom_codes(T,Cs).
canonical_to_atomic(i(T),T).
canonical_to_atomic(r(Cs),T):-number_codes(T,Cs).

compound_to_canonical(T,f(CTs)):-'=..'(T,Ts),map(to_canonical,Ts,CTs).

from_compound_canonical(f(CTs),T):-maplist(from_canonical,CTs,Ts),'=..'(T,Ts).

to_goedel(T,t(N,Gs)):-copy_term(T,CT),numbervars(CT,0,N),to_canonical(CT,Gs).

