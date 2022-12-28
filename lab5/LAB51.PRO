trace
domains
    list=integer*
    listOfList=list*
predicates
 
   isPal(list)
   equil(list, list)
    revert(list, list, list)
    append(list, list, list)
    getIndexesPal(list, list)
    checkPal(list)
    length(list, integer)
    between(integer, integer, integer)

    clear_pos(list, integer, list, list)
    make_zero_pal(list, list)
    appendListOfList(listOfList, list)
    


clauses 
    between(Low, Hight, _Value):-Low > Hight, !, fail.
    between(Low, Hight, Low).
    between(Low, Hight, Value):-
        NextLow = Low + 1, 
        between(NextLow, Hight, Value).
    revert([], L, L).
    revert([H|Tail], L, Res) :-
        revert(Tail, [H|L], Res).
    equil(L, L).
    isPal(L) :-length(L, Len), Len > 2, revert(L, [], Res), equil(L, Res).
    append([], L, L).
    append([H1|T1],L2 , [H1 | T]) :- append(T1, L2, T).
    appendListOfList([], []).
    appendListOfList([H|T], R):-
    	appendListOfList(T, R0),
    	append(H, R0, R).
    length([], 0).
    length([H|T], N) :- length(T, N1), N=N1+1.
    getIndexesPal(L, Res) :-
        append(X, Y, L),
        append(X1, _, Y),
        length(X1, Len),
        Len > 2,
        isPal(X1),
        length(X, From),
        To = From + Len - 1,
        findAll(N, between(From, To, N), Res).

    make_zero_pal(Xs, Ys) :-
        findall(Ns, getIndexesPal(Xs, Ns), Ns1),
        appendListOfList(Ns1, Ns2),
        clear_pos(Xs, 0, Ns2, Ys),

    clear_pos([], _, _, []).
    clear_pos([X | Xs], N, [N | Ns], [X | Ys]) :-
        N1 = N + 1,
        !,
        clear_pos(Xs, N1, Ns, Ys).
    clear_pos([_ | Xs], N, Ns, [0 | Ys]) :-
        N1 = N + 1,
        clear_pos(Xs, N1, Ns, Ys).

  
 
    checkPal(L) :- length(L, N), N > 2, not(isPal(L)).
     
     
     
     





   


 %goal
 	%palindrom([1, 2, 3, 4, 3, 5], ResList),
    %write(ResList). % 0, 0, 3, 4, 3, 0


    % вариант 7
    % 6 вариант6

    %формировани начального дерева
    %передача в целевой предикат поиска элемента
    %вывод дерева write




 	