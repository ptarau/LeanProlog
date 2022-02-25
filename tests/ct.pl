xc(_).


catchtest0:-
   G=((
     member(X,[xc(1),xc(2),nosuchpred(666),xc(3)]),
     topcall(X))
   ),
   G,
   println(X),
   fail.
    
catchtest1:-
   G=((
     member(X,[xc(1),nosuchpred(666),xc(3)]),
     topcall(X))
   ),
   new_engine(G,G,E),
   get(E,_),
   get(E,A),
   println(A),
   stop(E),
   fail.

catchtest2:-
   G=(
     member(_,[xc(1),xc(2),xc(3)])
   ),
   new_engine(G,G,E),
   element_of(E,A),
   println(A),
   fail.

catchtest3:-
   catch(
     member(X,[a,b,c]),
     boo, % not a match - should stop after first 2
     member(X,[10,20])
   ),
   println(X),
   fail.
   
catchtest4:-
   catch(
     member(X,[a,b,c]),
     _Any, % not a match - should stop after first 2
     member(X,[10,20])
   ),
   println(X),
   fail.
      
               
catchtest5:-
   catch(((
     member(X,[xc(1),xc(2),nosuchpred(666),xc(3)]),
     topcall(X))),
     boo, % not a match - should stop after first 2
     member(X,[10,20])
   ),
   println(X),
   fail.
   
   
catchtest6:-
   catch(((
     member(X,[xc(1),xc(2),nosuchpred(666),xc(3)]),
     topcall(X))),
     error(A,B),
     member(X,[10,20])
   ),
   (nonvar(A)->println(got_error(A,B));true),
   println(X),
   fail.

catchtest7:-
   catch(
    ((
     member(X,[xc(1),xc(2),nosuchpred(888),xc(3)]),
     topcall(X)
    )),
    error(A,B), % a match
    catch(catchtest5,boo,println(caught(error(A,B))))
   ),
   println(finally(A+B)),
   fail.
   
catchtest8:-
   catch(
    ((
     member(X,[xc(1),xc(2),nosuchpred(888),xc(3)]),
     topcall(X)
    )),
    Error,
    catch(catchtest5,Error1,println(caught(Error)))
   ),
   println(finally_ab(Error)),
   println(finally_cd(Error1)),
   fail.

topcall(X):-X.
println(X):-write(X),nl.


