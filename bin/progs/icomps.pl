% A000079=a(n)=2^n-1 Number of compositions (ordered partitions) of n into distinct parts
	
count_icomps(N,Count):-
  length(Xs,N),
  count_answers(icomps(Xs,_),Count).
  
icomps(Xs,[Xs]).
icomps([A,X|Xs],[[A|As]|Rss]):-
  append(As,[B|Bs],[X|Xs]),
  % add constraint on [A|As] here
  icomps([B|Bs],Rss).
  
