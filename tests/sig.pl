fix_zero(D,NewD):-Eps is 1/100000000,fix_zero(Eps,D,NewD).

fix_zero(Eps,D,NewD):-sign(D,Sign),abs(D,AD),AD=<Eps,!,NewD is  Sign*Eps.
fix_zero(_Eps,D,D).