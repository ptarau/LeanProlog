/*
trim_funs(FXs,FTs):-shallow_clone(FXs,FTs,N),trim_shallow(N,FXs,FTs).

trim_shallow(0,_,_):-!.
trim_shallow(I,FXs,FTs):-I>0,I1 is I-1,
  arg(I,FXs,X),arg(I,FTs,T),
  shallow_clone(X,T,_),
  trim_shallow(I1,FXs,FTs).

shallow_clone(FXs,FTs,N):-compound(FXs),!,
   arg(0,FXs,F),arity(FXs,N),fun(F,N,FTs).
shallow_clone(X,X,0).
*/

biglist(N,Xs):-numlist(0,N,Xs).

bug:-biglistassert(50000).

biglistassert(Size):-
  biglist(Size,D),
  abolish(big/1),
  %assert((big(X):-if(X=[],true,X=D))),
  assert(big(D)).

