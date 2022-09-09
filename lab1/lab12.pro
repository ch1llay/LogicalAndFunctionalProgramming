domains
predicates
    inv(integer, integer)
    invlor(integer, integer, integer)

clauses

    inv(0, 1).
    inv(1, 0).

    invlor(0, 0, 1).
    invlor(0, 1, 0).
    invlor(1, 0, 0).
    invlor(1, 1, 0).

goal
    write("Y "),
    readint(Y),
    invlor(X1, X2, Y),
    nl,
    write("return "), nl,
    write(X1), nl, write(X2), nl.
	