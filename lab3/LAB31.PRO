trace
 %  Определить число элементов списка, равным следующим за ним
domains
    list=integer*
predicates
    readlist(list)
    writelist(list)
    getpaircount(list, integer)
clauses
    readlist([Head|Tail]) :- write("elem "), nl,
    readint(Head), readlist(Tail), !.
    readlist([]).
    writelist([Head|Tail]) :-
     write(Head, " ") ,writelist(Tail), !.
   writelist([]).

    getpaircount([], 0).
   getpaircount([Head|[Head|Tail]], Count) :-
        getpaircount([Head|Tail],  Count1), 
        Count=Count1+1.
     getpaircount([_|Tail], Count) :-
    getpaircount(Tail, Count).
    

 %goal
 %	getpaircount([1, 2, 2, 2, 3, 3, 4], Count), nl,
 %	write(Count)

 	