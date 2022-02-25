:-op(1,fx,'f').
:-op(1,fx,'g').
:-op(1151,fx,'?').
:-op(1150,xf,'?').
:-op(200,xf,'~').
:-op(100,xfx,'~').
:-op(300,xf,'~').


t(X):-X=
  f (a,b,c, g (d,e,'f'), h). % should be 'f' because it is an op

go:-member(X,['?','~']),G=current_op(_,_,X),G,write(G),nl,fail.
go.
