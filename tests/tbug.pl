xn('666').
yn('''666''').
zn(3.14).
wn('3.14').

tbug:-Xs=[A,B,C],xn(A),yn(B),zn(C),maplist(type_of,Xs,Ts),traceln(Xs+Ts).
