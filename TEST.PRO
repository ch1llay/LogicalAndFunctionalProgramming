domains
predicates
	plus(integer, integer, integer)

clauses
    plus(X, Y, Z):-Z = X + Y.

goal
    write("X "), readint(X),
    write("Y "), readint(Y),
    plus(X, Y, Z), write("Z ", Z).ж