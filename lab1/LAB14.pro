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
    write("all variant"), nl,
    invlor(Y4, Y5, Y), inv(Y3, Y5), invlor(X3, X4, Y3), invlor(Y1, Y2, Y4), invlor(X2, X3, Y2), invlor(X1, X2, Y1),
    readchar(_),
    write(X1, " ", X2, " ", X3, " ", X4, " ", Y), nl, fail.
	