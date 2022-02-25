
process_ranked(B, C, D, E, I, O, F, J1) :-
	init_gensym(match),
	init_gensym(mismatch),
	(   get_param(freq_rank, A)
	->  true
	;   A=no
	),
	dprint(processing(syns=B, rel=C, co_occ(D, E))),
	(   vertex_of(F, G),
	    vertex_data(F, G, H),
	    (   H=N/J
	    ->  true
	    ;   dprint(unexpected=H),
		fail
	    ),
	    findall(L-K, compute_rank(A, I, J, G, K, L), M),
	    maxn(M, L-K),
	    update_rank(F, G, L),
	    add_edge(F, G, best_syn, N/K),
	    fail
	;   vertex_of(O, P),
	    vertex_data(O, P, Q),
	    findall(G-K/L, (member(G-_/J, Q), vertex_of(F, G), get_rank(F, G, L), edge_data(F, G, best_syn, K)), M),
	    add_edge(O, P, best_syn, M),
	    map(s2rank, M, R),
	    (   R=[]
	    ->  S is 0
	    ;   avg(R, S)
	    ),
	    update_rank(O, P, S),
	    fail
	;   first_nth(8, good_vertex_of(I, K), K),
	    get_rank(I, K, L),
	    i2ws(K, T),
	    fprintln(top_ranked_synset(K/L, T)),
	    fail
	;   vertex_of(O, P),
	    edge_data(O, P, best_syn, U),
	    fprintln(disambiguated_sentence=P),
	    vertex_data(O, P, H),
	    fprintln(rank_based_word_db_edge_data),
	    member(G-N/K/L, U),
	    arg(3, N, V),
	    (   integer(V),
		count_match(K, V),
		vertex_data(I, V, W),
		W=C
	    ->  get_rank(I, V, X)
	    ;   X=unranked
	    ),
	    i2ws(K, Z),
	    member(G-_/Y, H),
	    ftab(4),
	    fprintln([G, N, X, K, L, Y/Z]),
	    g(K, A1),
	    functor(W, def, 1),
	    once(member(W, A1)),
	    ftab(8),
	    fprintln(W),
	    fail
	;   rank_sort(O),
	    fprintln(ranked_sentences),
	    vertex_of(O, P),
	    get_rank(O, P, L),
	    fprintln(L:P),
	    fail
	;   extract_keyords(F, I),
	    fail
	;   true
	),
	gensym_no(mismatch, B1),
	E1 is B1+ -1,
	gensym_no(match, C1),
	D1 is C1+ -1,
	F1 is D1+E1,
	(   F1>0
	->  G1 is D1*100/F1
	;   G1=undefined
	),
	dprint(result(C, match_perc=G1, total=F1, match=D1, mismatch=E1)),
	dprint(---------------------------------------------------------------------),
	(   G1=undefined
	->  true
	;   gensym_no(file_count, _),
	    bb_val(perc, H1),
	    I1 is H1+G1,
	    bb_set(perc, I1)
	),
	enrich(I),
	J1=gs(O, F, I).
