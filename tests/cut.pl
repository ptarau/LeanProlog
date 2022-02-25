
a(X):-b(X)->c(X),!,c(X).

b(1).
b(2).

c(1).

aa(X):-b(X)->cc(X),!,cc(X);dd(X),!,dd(X).

cc(1).
dd(_):-write(dd),nl.

aaa(X):-bb(X)->ccc(X),!,ccc(X);dd(X),!,dd(X).

bb(_):-fail.
ccc(1).

z:-a(_)->b(X),!,b(X).

c:-[cut].

