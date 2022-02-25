  
external_goal(InFile,Goal):-
  traceln(calling(external_goal(InFile,Goal))),
  open(InFile,read,Interactor),
  jcall('vm.extensions.LineStreamInput',getReader(Interactor),Source),
  external_engine(Source,ok,(Goal,fail),E),
  get(E,A),
  stop(E),
  %println(external_got(A)), 
  arg(1,A,B),
  ( B==done -> true
  ;
    % println(B),
    arg(0,B,exception),
    arg(1,B,C),
    %println(external_engine_returned(C)),
    C=mcompile_and_stop(_,_) ->
    C
  ; errmes(external_error,external_top(InFile,Source))
  ).
    