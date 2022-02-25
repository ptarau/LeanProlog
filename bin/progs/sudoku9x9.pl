%   Sudoku in plain Prolog for older kids - working on 9x9 grid
%
%   author: Paul Tarau, Oct 2009
%   with data from program sudoku81.pl by Neng-Fa ZHOU
%
%   given that: 
%     var/nonvar can be expressed in terms of \+
%   and 
%     the DCG rules we used expand to pure Prolog
%   and
%     all call,map1,map2 can be replaced with first order
%     equivalents as they all are used with known arguments
%
%   => the language used here is basically pure Prolog extended with \+
    
go:-
    statistics(runtime,[Start|_]),
    sudoku(s9x9a,Solution),
    write(solved),nl,
    display_board(Solution),
    statistics(runtime,[End|_]),
    T is End-Start,
    write('execution time is '),write(T), write(milliseconds),nl.

sudoku(Problem,Xss):-
    call(Problem,Xsss),Xsss=[Xss,_,_],
    %display_board(Xss),
    preprocess([1,2,3,4,5,6,7,8,9],Xsss,XYss),
    %display_board(XYss),
    solve(XYss).
  
s9x9a([
    % horizontal
     [[A11,A12,A13,B11,B12,B13,C11,C12,C13],
      [A21,A22,A23,B21,B22,B23,C21,C22,C23],
      [A31,A32,A33,B31,B32,B33,C31,C32,C33],
      [D11,D12,D13,E11,E12,E13,F11,F12,F13],
      [D21,D22,D23,E21,E22,E23,F21,F22,F23],
      [D31,D32,D33,E31,E32,E33,F31,F32,F33],
      [G11,G12,G13,H11,H12,H13,I11,I12,I13],
      [G21,G22,G23,H21,H22,H23,I21,I22,I23],
      [G31,G32,G33,H31,H32,H33,I31,I32,I33]
     ],
    % vertical
     [[A11,A21,A31,D11,D21,D31,G11,G21,G31],
      [A12,A22,A32,D12,D22,D32,G12,G22,G32],
      [A13,A23,A33,D13,D23,D33,G13,G23,G33],
      [B11,B21,B31,E11,E21,E31,H11,H21,H31],
      [B12,B22,B32,E12,E22,E32,H12,H22,H32],
      [B13,B23,B33,E13,E23,E33,H13,H23,H33],
      [C11,C21,C31,F11,F21,F31,I11,I21,I31],
      [C12,C22,C32,F12,F22,F32,I12,I22,I32],
      [C13,C23,C33,F13,F23,F33,I13,I23,I33]
     ],
    % block
     [[A11,A12,A13,A21,A22,A23,A31,A32,A33],
      [B11,B12,B13,B21,B22,B23,B31,B32,B33],
      [C11,C12,C13,C21,C22,C23,C31,C32,C33],
      [D11,D12,D13,D21,D22,D23,D31,D32,D33],
      [E11,E12,E13,E21,E22,E23,E31,E32,E33],
      [F11,F12,F13,F21,F22,F23,F31,F32,F33],
      [G11,G12,G13,G21,G22,G23,G31,G32,G33],
      [H11,H12,H13,H21,H22,H23,H31,H32,H33],
      [I11,I12,I13,I21,I22,I23,I31,I32,I33]
     ]
   ]):-
    A12=6,B11=2,B13=4,C12=5,
    A21=4,A22=7,B22=6,C22=8,C23=3,
    A33=5,
    B32=7,
    C31=1,
    D11=9,E11=1,E13=3,F13=2,
    D22=1,D23=2,F23=9,
    D31=6,E31=7,E33=9,F33=8,
    G13=6,H12=8,I11=7,
    G21=1,G22=4,H22=9,I22=2,I23=5,
    G32=8,H31=3,H33=5,I32=9.

% preprocessor: propagates statically
% the known values and generates
% shortened permutation problems

preprocess(Is,Xsss,XYss):-
  interleave(Xsss,Yss),
  trimAllHints(Is,Yss,XYss).

% interleaves to ensure faster propagation
% by getting related variables closer
  
interleave(Xss,Xs):-interleave(Xss,Xs,[]).

interleave([[],[],[]])-->[].
interleave([[X|Xs],[Y|Ys],[Z|Zs]])-->[X],[Y],[Z],interleave([Xs,Ys,Zs]).

% generates shortened permutations by propagating hints
trimAllHints(Is,Xss,XYs):-trimAllHints(Is,Xss,XYs,[]).

trimAllHints(_,[])-->[].
trimAllHints(Is,[Xs|Xss])-->{trimHints(Xs,Ys,Is,Os)},[Os-Ys],trimAllHints(Is,Xss).

trimHints([],[],Is,Is).
trimHints([X|Xs],[X|Ys],Is,Os):-var(X),trimHints(Xs,Ys,Is,Os).
trimHints([X|Xs],Ys,Is,NewOs):-nonvar(X),sel(X,Is,Os),trimHints(Xs,Ys,Os,NewOs).

% solver: expects a list of lists containing elements Is-Xs
% with Is all known and Xs a mix of knowns and unknowns

solve(XYs):-map1(smart_permute,XYs).

smart_permute(Xs-Ys):-smart_permute(Xs,Ys).

smart_permute(Is,Xs):-trimHints(Xs,Ys,Is,Os),permute(Ys,Os).

permute([],[]).
permute(Xs,[X|Zs]):-sel(X,Xs,Ys),permute(Ys,Zs).

sel(X,[X|Xs],Xs).
sel(X,[Y|Xs],[Y|Ys]):-sel(X,Xs,Ys).

% I/O

display_board(Xss):-map1(writeOne,Xss),nl.

writeOne(Xs):-write(Xs),nl.

% portable utilities - as there are still good Prologs out there not having them :-)

map1(_,[]).
map1(F,[X|Xs]):-call(F,X),map1(F,Xs).

map2(_,[],[]).
map2(F,[X|Xs],[Y|Ys]):-call(F,X,Y),map2(F,Xs,Ys).

% nonvar_(X):- \+(X=0).

/*
solved
[8,6,1,2,3,4,9,5,7]
[4,7,9,5,6,1,2,8,3]
[3,2,5,9,7,8,1,6,4]
[9,5,8,1,4,3,6,7,2]
[7,1,2,8,5,6,3,4,9]
[6,3,4,7,2,9,5,1,8]
[5,9,6,4,8,2,7,3,1]
[1,4,3,6,9,7,8,2,5]
[2,8,7,3,1,5,4,9,6]

execution time is 2.2 seconds with SWI-Prolog, 12.3 million inferences
*/
