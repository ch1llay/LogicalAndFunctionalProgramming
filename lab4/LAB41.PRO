trace
domains
    list=integer*
predicates
    readlist(list, list, list)
    writelist(list)
    insert(list, integer, list)
    insertBegin(list, integer, list)
    insertEnd(list, integer, list)
    %listEmpty(list)
clauses
    
    readlist([H|T], L, Res) :- write("elem"), nl, readint(H), readlist(T, [H|L], Res).
    readlist([], L, L).

    writelist([Head|Tail]) :-write(Head, " ") ,writelist(Tail), !.
   writelist([]).

    insertBegin(L, El, [El|L] ).
    insertEnd([], El, [El]).
    insertEnd([L1|H1],E,[L1|H2]):-insertEnd(H1,E,H2).
    insert([], El, [El]).
    insert(List, El, Res) :- insertBegin(List, El, TempRes),
     insertEnd(TempRes, El, Res ).
    

 goal
 	readlist(List, [], Res),
 	writelist(Res),
 	insert(List, 3, Res1), nl,
 	write(Res1).

 	