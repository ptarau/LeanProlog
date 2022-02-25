:-['bits.pro'].

/* simple synthetizers, try:

?-syn((A*B)+(~C)).

?-syn([=>],[0],[A,B]:[A^B,B<A,B>A]).

?-syn(3:29). % <number of vars>:<truth table as integer>

?-syn(2:[12,6]).
*/

syn(E):- syn([<, =>],[0,1],E).

syn(Fs,E):-syn(Fs,[],E).

syn(Fs,Cs,E):- expr2tt(E,NV:TT),syn(Fs,Cs,NV,TT).

syn(Fs,Cs,NV,TT):-
  init_tts(NV,TT, MG,TTs),
  syn(NV,MG,Fs,Cs,TTs).
  
% synthesize and print result TTs list of numbers
syn(NV,MG,Fs,Cs,TTs):-
  portray_clause(syn(NV,MG,Fs,Cs,TTs)),
  show_tts(NV,TTs),
  statistics(runtime,[T1,_]),
  syn(NV,MG,Fs,Cs,TTs, R),
  !,
  statistics(runtime,[T2,_]),
  portray_clause(R),
  T is T2-T1,
  write(time_ms=T),nl,
  fail.
  
% synthesize and group results in a more readable form
syn(NV,MG, Fs,Cs,TTs, Xs:Gs=Ys:TTs):-
  syn(NV,MG,Fs,Cs,TTs, Xs,Gs,Ys).

% synthetiser API - input Fs=lib,Cs=consts,E=expression
%  output: TTs=ttable, Xs=input vars,Gs=DAG,Ys=output vars
syn(Fs,Cs,E, TTs, Xs,Gs,Ys):-
  expr2tt(E,NV:TTs0),
  init_tts(NV,TTs0, MG,TTs),
  syn(NV,MG,Fs,Cs,TTs, Xs,Gs, Ys).

% synthetisize a circuit, and interprets results as a DAG
syn(NV,MG,Fs,Cs, TTs, Xs,Gs,Ys):-
  synthetize_circuit(NV,MG,Fs,Cs,TTs,Vs,Gs0),
  !,
  showdag(NV,Cs,Vs,Gs0, Xs,TTs,Ys, Gs).

% given: 
%   NV variables, MG max gates, functions Fs, constants Cs,
%   a list of output truth table vectors TTs
% => build a DAG as:
%   Vs, a list of NV primary input variables
%   Gs a list of gates evaluating to each of the TTs

synthetize_circuit(NV,MG,Fs,Cs,TTs, Vs,Gs):-
  initInputs(NV,Cs,M,Vs,Is),
  try_gates(MG,M,Fs,TTs,Is,Gs,_,[]).

% enumerates circuits in increasing order
enumerateCircuits(_MG,_M,_Fs,Is, Is,[TT]):-
  % when outputs connect directly to inputs
  member(TT,Is).
enumerateCircuits(MG,M,Fs,Is, Gs,Os):-
  % when gates connect inputs to outputs
  generate_gates(MG,M,Fs,[_AnyTT],Is, Gs,Os,_). 

try_gates(NG,M,Fs,TTs,Is, Gs,Os,NewTTs):-
  trim_tts(TTs,Is,_SolvedTTs,UnSolvedTTs),
  generate_gates(NG,M,Fs,UnSolvedTTs,Is, Gs,Os,NewTTs).
        
% generates gates
generate_gates(_,_,_,TTs,Is, Is,Is,TTs).
generate_gates(NG,M,Fs,TTs,Is, [G|Gs],[VO|Os],NewTTs):-
  NG>0,NG1 is NG-1,
  generate_gates(NG1,M,Fs,TTs,Is, Gs,Os,OldTTs),
  newGate(Fs,M,Os,G,VO),
  check_progress(VO,OldTTs,NewTTs).

% pick a gate, connect it and evaluate it  
newGate(Fs,M,Os,G,VO):-
  member(F,Fs),
  newGateByArity(F,M,Os,G,VO),
  % gate already generated
  \+(member(VO,Os)).
  
% arity 1 gate - negation
newGateByArity(F,M,Os,g(F,VK,VO),VO):-F='~',!,
  member(VK,Os),
    applyF(F,M,VK,VO).
% arity 3 gate - if-then-else 
newGateByArity(F,M,Os,g(F,VK,VI,VJ,VO),VO):-F=ite,!,
  member(VK,Os),
    member(VI,Os),
      member(VJ,Os),
       applyF(F,M,VK,VI,VJ,VO). 
% 16 arity 2 gates
newGateByArity(F,M,Os,g(F,VI,VJ,VO),VO):-
    member(VI,Os),
      member(VJ,Os),
       applyF(F,M,VI,VJ,VO).

% trims truth table lists of plain inputs - they need no search         
trim_tts(TTs,Is,SolvedTTs,UnSolvedTTs):-
  findall(X,(member(X,TTs),member(X,Is)),SolvedTTs),
  findall(X,(member(X,TTs),\+member(X,Is)),UnSolvedTTs).

%check_progress(VO,Os,_OldTTs,_NewTTs):-member(VO,Os),!,fail.
check_progress(VO,OldTTs,NewTTs):-pick(VO,OldTTs,More),!,NewTTs=More.
check_progress(_VO,TTs,TTs).

% selects/inserts a value from/to a list
pick(X,[X|Xs],Xs).
pick(X,[Y|Xs],[Y|Ys]):-pick(X,Xs,Ys).
         