% ystate.pl

/*
  Abstract dynamic database API using engines as server agents
*/

open_state(S):-new_state(S),set_val('$state',S),protect_engine(S).

close_state(S):-remove_val('$state'),unprotect_engine(S),stop(S).

default_state(S):-get_val('$state',S).

set_state(X,A):-default_state(S),set_state_of(S,X,A).

get_state(X,A):-default_state(S),get_state_of(S,X,A).

all_state(X,A):-default_state(S),all_state_of(S,X,A).

remove_state(X):-default_state(S),remove_state_of(S,X).

% creates a new engine server providing state operations
new_state(State):-new_engine(done,state_server,State).
% sets an element to a value
set_state_of(State,X,A):-ask_engine(State,set_state(X,A),the(_)).
% gets the value of an element
get_state_of(State,X,A):-ask_engine(State,get_state(X),the(found(A))).
% backtracks over state variables and their values
all_state_of(State,X,A):-
  ask_engine(State,all_state,the(XAs)),
  reverse(XAs,Rs),
  member((X-A),Rs).
% makes a state variable undefined
remove_state_of(State,X):-
  ask_engine(State,remove_state(X),the(found)).
% removes a state space
state_delete(State):-stop(State).

/*
  Helper predicates/Implementation
*/
state_server:-state_server([]).
    
state_server(S1):-
  from_engine(Q),
  state_server_task(Q,S1,S2,A),
  return(A),
  state_server(S2).

%state_server_task(Op,S1,_,_):-traceln(state_server(Op,S1)),fail.
state_server_task(set_state(X,A),Xs,[X-A|Ys],New):-
  (
    select(X-_,Xs,Ys),!,New=old
  ; New=new,Ys=Xs
  ).
state_server_task(get_state(X),Xs,Xs,R):-
  (
    member(X-A,Xs),!,R=found(A)
  ; R=not_found
  ).
state_server_task(remove_state(X),Xs,Ys,R):-
  ( select(X-_,Xs,Ys),!,R=found
  ; Ys=Xs,
    R=not_found
  ).
state_server_task(all_state,Xs,Xs,Xs).


