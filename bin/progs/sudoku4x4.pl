% Sudoku for kids in pure Prolog
% author: Paul Tarau, Sept 2009
% license: free to all

s4x4([
  [
     [S11,S12, S13,S14],
     [S21,S22, S23,S24],

     [S31,S32, S33,S34],
     [S41,S42, S43,S44]
  ],
  [
     [S11,S21, S31,S41],
     [S12,S22, S32,S42],

     [S13,S23, S33,S43],
     [S14,S24, S34,S44]
  ],
  [
     [S11,S12, S21,S22],
     [S13,S14, S23,S24],

     [S31,S32, S41,S42],
     [S33,S34, S43,S44]
  ]
]):-
  % here you can add clues like S11=3, S41=2, etc.
  % tested with SWI-Prolog - uses maplist/2
  true.

% this generates the 288 puzzles - and solves one if given clues
sudoku(Xss):-
  s4x4(Xsss),Xsss=[Xss|_],
  maplist(maplist(permute([1,2,3,4])),Xsss).
 
permute([],[]).
permute([X|Xs],Zs):-permute(Xs,Ys),ins(X,Ys,Zs).

ins(X,Xs,[X|Xs]).
ins(X,[Y|Xs],[Y|Ys]):-ins(X,Xs,Ys).

% test
go:-sudoku(Xss),
    nl,member(Xs,Xss),
    write(Xs),nl,fail
  ; nl.
  
sudokus(Total-Xsss):-
  findall(Xss,sudoku(Xss),Xsss),
  length(Xsss,Total).

c:-[sudoku].

test1:-
 statistics(runtime,_),
  (sudoku(_),fail;true),
 statistics(runtime,[_,T]),
 write(T),nl.

test2:-
 statistics(runtime,_),
  (sudokus(_),fail;true),
 statistics(runtime,[_,T]),
 write(T),nl.


