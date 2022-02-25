g(100027807,'the; "geometry "').
g(112479689,  'dragon''s blood. '). % ok
% 'something
g(1001546899999999,'it\'s something 	e.g. gold').
g(100523263,'/* ok */ William F. Cody').
g(103608870,'leaves "85% of"'). /*
a comment*/
       g(aaa,"x. ""y", f(a, b)).

bof :-
	Key =.. ['.'|a],
	functor(Key, '.', 2).

deep :- shallow(_X, 100). % initially b(_X, 100) 
shallow :- deep(_X, 100).	% initially the same


check_return(R,X):-nonvar(R),X='$fou\'nd'(A),!,X=A.

end.
