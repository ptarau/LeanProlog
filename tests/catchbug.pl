% FIXED catbug:-catch(throw(boo),E,traceln(E)),traceln('SHOULD_GET_HERE').
 
badthrow:-between(1,3,I),throw(I),fail.
badcatch:-catch(badthrow,X,(write(X),nl,fail)).
 
go:-badcatch.
