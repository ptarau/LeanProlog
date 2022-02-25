fa1(M, K, P) :- N is M + K, +(N, 2.1, P).

bug:-fa1(3.14,0.12,R),println(R).

bad(3.14).
