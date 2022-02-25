:-op(600,xfx,('<=')).

% example data - clauses are binarized

clauses([
	app([],A,A,B)<=B,
	app([C|D],E,[C|F],G)<=app(D,E,F,G),

	nrev([],[],H)<=H,
	nrev([I|J],K,L)<=nrev(J,M,app(M,[I],K,L)),

	perm([],[],N)<=N,
	perm([O | P],Q,R)<=perm(P,S,ins(O,S,Q,R)),

	ins(T,U,[T | U],V)<=V,
	ins(W,[X | Y],[X | Z],X0)<=ins(W,Y,Z,X0)
]).

% sketch of a proof-of-concept metaintepreter implementing logic engines
% in side effect free Prolog 
%
% note that in theory CUT in this code can be expressed in terms of 
% if-then-else or negation as failure
%
% limitations:
%   clauses in the database need to be converted to a list of binarized clauses
%   return, to_engine/from_engine not yet supported
%   assumes finite number of answers - that explores one at time with get
%   the code can be transformed ot coroutine by passing it
%   the client's current continuation as a callback
%
%   given that the resolvent is repeatedly copied this is excruciatingly slow
%

meta_new_engine(Answer,Goal,Clauses,Engine):-
  Engine=new_engine(Answer,Goal,Clauses).
  
meta_get(Engine,NewEngine,R):-
  Engine=new_engine(Answer,Goal,Clauses),
  compute_answers(Answer,Goal,Clauses,Answers),
  Answers=[Answer|MoreAnswers],
  !,
  NewEngine=done_engine(MoreAnswers),
  R=the(Answer).
meta_get(Engine,NewEngine,R):-
  Engine=done_engine(Answers),
  Answers=[Answer|MoreAnswers],
  !,
  NewEngine=done_engine(MoreAnswers),
  R=the(Answer).
meta_get(_Engine,stopped_engine,no).

meta_stop(_Engine,stopped_engine).

% interface to emulated all_solutions predicate

all_(G,R):-clauses(C),all_instances(G,C,R).

all_instances(Goal,Clauses,Answers):-
        derive_all([Goal<=Goal],Clauses,[],Answers).

% derives all answers until there is no Arrow=(Answer<=Goal) left

compute_answers(AnswerPattern,Goal,Clauses,Answers):-
  derive_all([AnswerPattern<=Goal],Clauses,[],Answers).

derive_all([],_,As,As).
derive_all([Arrow|Fs],Cs,OldAs,NewAs):-
  derive_one(Arrow,Cs,Fs,NewFs,OldAs,As),
  derive_all(NewFs,Cs,As,NewAs).

% if Answer<=true has been deduced then keep answer
% else replace Answer<=Goal with its consequences Answer<=Body
% obtained from input clauses of the form Goal<=Body

derive_one(Answer<=true,_,Fs,Fs,As,[Answer|As]):-
  % portray_clause(Answer),
  true.
derive_one(Answer<=Goal,Cs,Fs,NewFs,As,As):-Goal\==true,
  match_all(Cs,Answer<=Goal,Fs,NewFs).

match_all([],_,Fs,Fs).
match_all([Clause|Cs],Arrow,Fs1,Fs3):- 
   match_one(Arrow,Clause,Fs1,Fs2),
   match_all(Cs,Arrow,Fs2,Fs3).

% basic inference step

match_one(F1,F2,Fs,[F3|Fs]):-compose(F1,F2,F3),!.
match_one(_,_,Fs,Fs).

compose(F1,F2,A<=C):-pcopy_term(F1,A<=B),pcopy_term(F2,B<=C).

% pure copy_term

pcopy_term(T,CT):-pcopy_term(T,_Dict,CT).

pcopy_term(X,D, V):-var(X),!,lookup_var(X,D,V).
pcopy_term(T,_, CT):-atomic(T),!,CT=T.
pcopy_term(T,D, CT):-
  functor(T,F,N),
  functor(CT,F,N),
  pcopy_args(N,T,D,CT).

pcopy_args(0,_,_,_):-!.
pcopy_args(I,T,D,CT):-
  I1 is I-1,
  arg(I,T,X),
  arg(I,CT,A),
  pcopy_term(X,D,A),
  pcopy_args(I1,T,D,CT).

/*
% note that ve emulate here a simpler use of var + ==
same_vars(0,1):-!,fail.
same_vars(1,0):-!,fail.
same_vars(_,_).
*/

lookup_var(X,Vs,V):-var(Vs),!,Vs=[X-V|_].
lookup_var(X,[Y-U|_],V):-same_vars(X,Y),!,V=U.
lookup_var(X,[_|Vs],V):-lookup_var(X,Vs,V).

mytime(G,T):-statistics(runtime,[T1,_]),G,!,statistics(runtime,[T2,_]),T is T2-T1.

% examples

i1:-G=nrev([a(X),b,c(X)],_,true), all_(G,R),write(R),nl.

i2:-G=app(_,_,[a,b],true), all_(G,R),statistics,write(R),nl.

i3:-G=perm([a,b,c],_,true), all_(G,R),statistics,write(R),nl.

i4:-G=perm([a,b,c],P,true), clauses(Cs),compute_answers(P,G,Cs,As),statistics,write(As),nl.

i5:-metatest.
     
metatest:-G=perm([a,b,c],P,true), clauses(Cs),
    meta_new_engine(P,G,Cs,E),
    meta_get(E,E1,A),
    meta_get(E1,E2,B),
    meta_stop(E2,E3),
    meta_get(E3,E4,C),
    write(A),nl,
    write(B),nl,
    write(C),nl,
    write(E3+E4),nl.
     
integers([],I,I):-!.
integers([I0|L],I0,I):-I0<I,I1 is I0+1,integers(L,I1,I).

% benchmarks

bm(N):-
	Len is N+1,
	integers(Is,1,Len),
	G=nrev(Is,_,true), 
     mytime(all_(G,_),T),
     L is ((N+1)*(N+2))/(2*T),
     Lips is 1000*L,
     write([time(T),lips(Lips)]),nl.
     
go:-bm(20).
