make_zero_pal(Xs, Ys) :-
	findall(Ns, gen_pal(Xs, Ns), Ns1),
    append(Ns1, Ns2),
    sort(Ns2, Ns3),
    clear_pos(Xs, 0, Ns3, Ys).

clear_pos([], _, _, []).
clear_pos([X | Xs], N, [N | Ns], [X | Ys]) :-
    N1 is N + 1,
    !,
	clear_pos(Xs, N1, Ns, Ys).
clear_pos([_ | Xs], N, Ns, [0 | Ys]) :-
    N1 is N + 1,
	clear_pos(Xs, N1, Ns, Ys).

gen_pal(Xs, Ns) :-
    append(Xs1, Xs2, Xs),
    append(Ps, _, Xs2),
    length(Ps, Len),
    Len > 2,
    is_pal(Ps),
    length(Xs1, From_),
    To_ is From_ + Len - 1,
    findall(N, between(From_, To_, N), Ns).

is_pal(Xs) :-
    append(Xs1, Xs2, Xs),
    reverse(Xs1, Xs11),
    ( Xs11 = Xs2 ; Xs11 = [_ | Xs2] ).

