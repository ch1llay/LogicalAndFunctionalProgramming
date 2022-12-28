trace
domains
    treetype = tree(integer, treetype, treetype); empty
 
 
predicates
    max(integer,integer,integer)
    max_el(treetype,integer)
    replace(treetype,integer,treetype)
    repl_neg(treetype,treetype)
 
clauses
max(M,N,N) :- N>=M, !.
max(M,N,M) :- M>=N, !.
 
max_el(tree(N,empty,empty),N).
max_el(tree(N,empty,TR),Q) :-
	 max_el(TR,M), max(N,M,Q).
max_el(tree(N,TL,empty),Q) :-
	 max_el(TL,M), max(N,M,Q).
max_el(tree(N,TL,TR),Q) :-
	 max_el(TL,QL),max_el(TR,QR),
	 max(QL,QR,M),max(N,M,Q).
 
replace(tree(N,empty,empty)
,N,tree(N,empty,empty)) :- N>=0.
replace(tree(N,empty,empty)
,K,tree(K,empty,empty)) :- N<0.
replace(tree(N,TL,TR),_,tree(N,TTL,TTR)) :-
	 N>=0,replace(TL,K,TTL), replace(TR,K,TTR).
replace(tree(N,TL,TR),K,tree(K,TTL,TTR)) :-
	 N<0,replace(TL,K,TTL), replace(TR,K,TTR).
 
repl_neg(T,Q):- max_el(T,K), replace(T,K,Q).
 
goal
repl_neg
(tree(-6
	,tree(8,
		tree(-7,empty,empty),
		tree(18,empty,empty))
,	tree(-1,empty,empty)),Z),
write(Z),nl.