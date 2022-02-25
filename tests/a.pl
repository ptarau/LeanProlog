:-['b.pl'].

a(X):-b(X). % b steals all defs after
a(4).
a(5).
a(X):-c(X).

:-index(c(1)).
:-op(100,fx,'c'). % in case already and op: should be 'c'

c 6.
c 7.


