% :-['../psrc/top.pl']. see y.pl

:-hi.
:-initialization(xgo).
:-initialization(println(one)).
:-initialization(qx(_)).

% :-set_verbosity(10,_).

%:-dynamic(dx/1).
:-index(dx(1)).

:-dynamic(qx/1).

dx(aaa).
dx(bbb).
dx(ccc).

qx(111).
qx(222).

% :-symgc.
:-hi.

sx(X):-dx(X).
sx(X):-qx(X).

xgo:-sx(X),println(X),fail;true.


boo:-bad(_).
boo:-xgo.


% this can be anywhere - executed after all code loaded
%:-xgo.
