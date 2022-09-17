domains
predicates
    inv(integer, integer)
    invlor(integer, integer, integer)
    p(integer)

clauses

    inv(0, 1).
    inv(1, 0).

    invlor(0, 0, 1).
    invlor(0, 1, 0).
    invlor(1, 0, 0).
    invlor(1, 1, 0).
    P(1).
    P(0).

goal
    write("all variant"), nl, P(Y),
    invlor(X1, X2, Y1), invlor(X2, X3, Y2), invlor(Y1, Y2, Y4),
    invlor(X3, X4, Y3), inv(Y3, Y5), invlor(Y4, Y5, Y),
    readchar(_),
    write(X1, " ", X2, " ", X3, " ", X4, " ", Y), nl, fail.
	