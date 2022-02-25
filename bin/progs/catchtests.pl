
catchtest0:-rcatch(10,X),xwriteln(X).
catchtest1:-rcatch1(10,X),xwriteln(X).
catchtest2:-rcatch2(10,X),xwriteln(X).

catchtest3:-
   catch(
     member(X,[a,b,c]),
     boo, % not a match - should stop after first 2
     member(X,[10,20])
   ),
   xwriteln(X),
   fail.
   
catchtest4:-
   catch(
     member(X,[a,b,c]),
     _Any, % not a match - should stop after first 2
     member(X,[10,20])
   ),
   xwriteln(X),
   fail.
      
               
catchtest5:-
   catch(((
     member(X,[xc(1),xc(2),nosuchpred(666),xc(3)]),
     xtopcall(X))),
     boo, % not a match - should stop after first 2
     member(X,[10,20])
   ),
   xwriteln(X),
   fail.
   
   
catchtest6:-
   catch(((
     member(X,[xc(1),xc(2),nosuchpred(666),xc(3)]),
     xtopcall(X))),
     error(A,B),
     (xwriteln(got_error(A,B)),member(X,[10,20]))
   ),
   xwriteln(X),
   fail.

catchtest7:-
   catch(
    ((
     member(X,[xc(1),xc(2),nosuchpred(888),xc(3)]),
     xwriteln(X)
    )),
    error(A,B), % a match
    catch(catchtest5,boo,xwriteln(caught(error(A,B))))
   ),
   xwriteln(finally(A+B)),
   fail.
   
catchtest8:-
   catch(
    ((
     member(X,[xc(1),xc(2),nosuchpred(888),xc(3)]),
     xtopcall(X)
    )),
    Error,
    catch(catchtest5,Error1,xwriteln(caught(Error)))
   ),
   xwriteln(finally_ab(Error)),
   xwriteln(finally_cd(Error1)),
   fail.

catchtest9:-rsubset([1,2,3,4],R),xwriteln(R),fail;true.

catchtest10:-
  catch(once(member(_A,[1,2])),_,true),
  engines(X),
  println(engines(X)).
  
% these leave garbage

catchtest11:-
  catch(xc(_),_,true),
  engines(X),
  println(engines(X)).


  
% auxiliary preds
        
rcatch(0,1).
rcatch(N,X):-N>0,N1 is N-1,catch(nosuchfun(N),_Ignore,rcatch(N1,Y)),X is N*Y.

rcatch1(0,1).
rcatch1(N,X):-N>0,N1 is N-1,catch(nosuchfun(N),_Ignore,true),rcatch1(N1,Y),X is N*Y.

rcatch2(0,1).
rcatch2(N,X):-N>0,N1 is N-1,catch(nosuchfun(N),nosuchexc,rcatch2(N1,Y)),X is N*Y.

rsubset([],[]).
rsubset([_|Xs],Ys):-catch(nosuchfun(Xs),_Any,rsubset(Xs,Ys)).
rsubset([X|Xs],[X|Ys]):-catch(rsubset(Xs,Ys),_Any,true).


    
xc(_).
xwriteln(X):-write(X),nl.
xtopcall(X):-call(X).

% lean Prolog only - involving engines

entest0:-
   G=((
     member(X,[xc(1),xc(2),nosuchpred(666),xc(3)]),
     xtopcall(X))
   ),
   G,
   xtopcall(X),
   fail.

entest1:-
   G=((
     member(X,[xc(1),nosuchpred(666),xc(3)]),
     xtopcall(X))
   ),
   new_engine(G,G,E),
   get(E,_),
   get(E,A),
   traceln(got(A)),
   traceln(here),
   stop(E),
   xwriteln(A),
   fail.

entest2:-
   G=(
     member(_,[xc(1),xc(2),xc(3)])
   ),
   new_engine(G,G,E),
   element_of(E,A),
   xtopcall(A),
   fail.

% catchtests for any prolog

 