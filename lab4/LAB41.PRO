
domains
    list=integer*
predicates
    readlist(list, list)
    writelist(list)
    insert(list, integer, list)
    insertBegin(list, integer, list)
    insertEnd(list, integer, list)
    %listEmpty(list)
clauses
    
    readlist(L, R) :- write("elem"), nl, readint(H), readlist([H|L], R).
    readlist(L, L).

    writelist([Head|Tail]) :-write(Head, " ") ,writelist(Tail), !.
   writelist([]).

    insertBegin(L, El, [El|L] ).
    insertEnd([], El, [El]).
    insertEnd([L1|H1],E,[L1|H2]):-insertEnd(H1,E,H2).
    insert([], El, [El]).
    insert(List, El, Res) :- %insertBegin(List, El, TempRes),
     insertEnd([El|List], El, Res ).
    

 goal
 	readlist([], Res),
 	writelist(Res), nl,
 	write("element for insert"), nl,
 	readint(El),
 	insert(Res, El, Res1), nl,
 	write(Res1).

 	