ps2rank([],0).
ps2rank([X|Xs],R):-ps2rank(X,Rx),ps2rank(Xs,Rxs),cons(Rx,Rxs,R).

cons(X,Y,XY):-X>=0,Y>=0,XY is (2**X)*(2*Y+1).
hd(XY,X):-XY>0,P is XY mod 2,hd1(P,XY,X).

hd1(1,_,0).
hd1(0,XY,X):-Z is XY // 2,hd(Z,H),X is H+1.

tl(XY,Y):-hd(XY,X),Y is XY // (2**(X+1)).

null(0).

fun2nat([],0).
fun2nat([X|Xs],N):-fun2nat(Xs,N1),cons(X,N1,N).

nat2fun(0,[]).
nat2fun(N,[X|Xs]):-N>0,hd(N,X),tl(N,T),nat2fun(T,Xs).

term2ps(T,Ps,As):-term2ps(T,Ps,As,[]).

term2ps(T,[])-->{var(T)},[T].
term2ps(T,[])-->{atomic(T)},[T].
term2ps(T,Ps)-->{compound(T),T=..Xs},
  args2ps(Xs,Ps).
  
args2ps([],[])-->[].
args2ps([X|Xs],[P|Ps])-->
  term2ps(X,P),
  args2ps(Xs,Ps). 

  

% ?- lcons(H,T,[0,0,1, 0,1,  0,1,1]),cons(H,T,Ps).  
lcons(Hs,[0|Ts],[0|Ps]):-pars(Hs,Ps,Ts).
  
pars([0|Xs]) --> [0],lpars(Xs).

lpars([1])-->[1],!.
lpars([X|Xs])-->pars(X),lpars(Xs).
  
% ?- tpars("(()((12))(aaa))",T).

tpars(Xs,T) :- "()"=[L,R],tpars(L,R,T,Xs,[]).

tpars(L,R,Xs) --> [L],lpars(L,R,Xs).

lpars(L,R,N) --> npars(L,R,Ns),[R],{Ns=[]->N=[];name(N,Ns)},!.
lpars(L,R,[X|Xs])-->tpars(L,R,X),lpars(L,R,Xs).

npars(L,R,[]) --> peek(X),{member(X,[L,R])},!.
npars(L,R,[X|Xs]) --> [X],npars(L,R,Xs).

peek(X,[X|Xs],[X|Xs]).



% part of pISO
term2pars(T,Ps,As):-term2pars(T,Ps,[],As,[]).

term2pars(T,Ps,Ps)-->{var(T)},[T].
term2pars(T,Ps,Ps)-->{atomic(T)},[T].
term2pars(T,[0|Ps],NewPs)-->{compound(T),T=..Xs},
  args2pars(Xs,Ps,NewPs).
  
args2pars([],[1|Ps],Ps)-->[].
args2pars([X|Xs],[0|Ps],NewPs)-->
  term2pars(X,Ps,[1|XPs]),
  args2pars(Xs,XPs,NewPs).  
  

pars2term(Ps,As,T):-pars2term(T,Ps,[],As,[]).

pars2term(T,Ps,Ps)-->[T].
pars2term(T,[0|Ps],NewPs)-->
  pars2args(Xs,Ps,NewPs),
  {T=..Xs}.
  
pars2args([],[1|Ps],Ps)-->[].
pars2args([X|Xs],[0|Ps],NewPs)-->
  pars2term(X,Ps,[1|XPs]),
  pars2args(Xs,XPs,NewPs).  


% conversion to list of digits in given base
to_base(Base,N,Bs):-to_base(N,Base,0,Bs).

to_base(N,R,_K,Bs):-N<R,Bs=[N].
to_base(N,R,K,[B|Bs]):-N>=R,
  B is N mod R, N1 is N//R,K1 is K+1,
  to_base(N1,R,K1,Bs).

% conversion from list of digits in given base
from_base(_Base,[],0).
from_base(Base,[X|Xs],N):-from_base(Base,Xs,R),N is X+R*Base.

go:-
  term2ps(f([a,b],A,g(A,B),B),Ps,As),
  ps2rank(Ps,R),
  %pars2term(Ps,As,T),
  write(R=Ps+As),nl.
  
go1:-
  term2pars(f([a,b,c],A,g(A,h(b,B)),B),Ps,As),
  pars2term(Ps,As,T),
  from_base(2,Ps,R),
  write(R:T=Ps+As),nl.
  
  
  