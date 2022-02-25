% writer.pl

% WRITER to DCG list

%unparse(T,Xs):-number(T),!,to_canstring(T,N),Xs=[N].
unparse(T,Xs):-atomic(T),!,Xs=[T].  
unparse(T,Xs):-unparse(T,Xs,[]).

unparse(T,Xs,End):-unparse(T,1200,Xs,End).

unparse(X,_)--> {var(X)},!,[X].
unparse('$VAR'(A),_)--> !,['_'],[A].
unparse([X|Xs],N)-->!,['['],unparse_list(Xs,X,N),[']'].
unparse('{}'(Xs),N)-->!,['{'],unparse(Xs,N),['}'].
unparse(FXs,N)--> {compound(FXs)},!,{FXs=..[F,X|Xs]},unparse_fun(F,X,Xs,N).
unparse(X,_)--> {maybe_quoted(X,QX)},[QX].

unparse_fun(F,X,[],N)-->
  {prefix(F,P,XP)},
  !,
  insPar('(',N,P),[F],[' '],
    unparse(X,XP),
  insPar(')',N,P).
unparse_fun(F,X,[],N)--> 
  {postfix(F,P,XP)},
  !,
  insPar('(',N,P),
    unparse(X,XP),[' '],[F],
  insPar(')',N,P).
unparse_fun(F,X,[Y],N)--> 
  {infix(F,P,LP,RP)},
  !,
  insPar('(',N,P),
    unparse(X,LP),
  [' '],[F],[' '],
    unparse(Y,RP),
  insPar(')',N,P).
unparse_fun(F,X,Xs,N)--> {maybe_quoted(F,QF)},
  [QF],['('],unparse_args(Xs,X,N),[')'].

unspace([],[]).
unspace([X|Xs],Ys):-' '=X,!,unspace(Xs,Ys).
unspace([X|Xs],[X|Ys]):-unspace(Xs,Ys).

%maybe_quoted(X,X).
maybe_quoted(X,QX):-quote_if_needed(X,QX).


quote_if_needed(A,AQA):-quote_if_needed0(A,R),!,AQA=R.
quote_if_needed(A,QAQ):-current_op(_,_,A),!,to_quoted(A,QAQ).
quote_if_needed(A,A).

quote_if_needed0('(', '''(''').
quote_if_needed0(')', ''')''').
quote_if_needed0('[', '''[''').
quote_if_needed0(']', ''']''').
quote_if_needed0('{', '''{''').
quote_if_needed0('}', '''}''').
%quote_if_needed0('/\\', '''/\\\\''').
%quote_if_needed0('\\/', '''\\\\/''').
%quote_if_needed0('\\', '''\\\\''').

to_quoted(X,QX):-atom_codes(Q,[39]),concat_atom([Q,X,Q],QX).

insPar(Par,N,P)-->{N<P},!,[Par].
insPar(_,_,_)-->[].

unparse_args([],X,N)--> !,unparse(X,N).
unparse_args([Y|Xs],X,N)--> unparse(X,N),[','],[' '],unparse_args(Xs,Y,N).

unparse_list(YXs,X,N)--> {var(YXs)},!,unparse(X,N),['|'],unparse(YXs,N).
unparse_list([],X,N)--> !,unparse(X,N).
unparse_list([Y|Xs],X,N)--> !,unparse(X,N),[','],[' '],unparse_list(Xs,Y,N).
unparse_list(YXs,X,N)--> unparse(X,N),['|'],unparse(YXs,N).

unvar(X-X).

untok([])-->[].
untok([T|Ts])-->
  untok1(T),[' '],
  untok(Ts).

untok1(T)-->{atomic(T)},!,[T].
untok1(qtd(X))-->!,{Q=39,atom_codes(SQ,[Q])},[SQ],[X],[SQ].
untok1(str(X))-->!,{Q=34,atom_codes(SQ,[Q])},[SQ],[X],[SQ].
untok1(T)-->{arg(1,T,X)},[X].

