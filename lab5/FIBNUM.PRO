trace
predicates 
   fibnum(integer, integer)
   
clauses
    fibnum(1, 1).
    fibnum(0, 0).
    fibnum(X, Y) :- X1 = X


goal
    fibnum(3, Res), write(Res).


1, 1, 2, 3, 5, 8, 13, 21


