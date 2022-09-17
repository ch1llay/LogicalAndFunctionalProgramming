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
    readint(Y), nl, write("all variant for ", Y),
    invlor(X1, X2, Y1), invlor(X2, X3, Y2), invlor(Y1, Y2, Y4),
    invlor(X3, X4, Y3), inv(Y3, Y5), invlor(Y4, Y5, Y),
    nl,
    write(X1, " ", X2, " ", X3, " ", X4), fail.
	