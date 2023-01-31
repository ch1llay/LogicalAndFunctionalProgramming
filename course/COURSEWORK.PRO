
operator(S) :-
    S='+';S='-';S='*';S='/'.

check([], _, _).
check([E|T], [HO|TO], Numbers):-
    operator(E), HO is E, check(T, [TO], Numbers).
check([E|T], Operations, [HN|TN]):-
    not(operator(E)), HN is E, check(T,Operations, [TN]).



translate(B):-write("enter excpression\n"), 
    read(A),
    processing(A,List),
    atom_chars(B,List).

processing(A,L):-
    atomic(A),
    atom_chars(A,L).
processing(A+B,['+','('|Tail]):-
    processing(A,A1),processing(B,B1), 
    append(A1,[','|B1],Temp),
    append(Temp,[')'],Tail).
processing(A-B,['-','('|Tail]):-
    processing(A,A1),processing(B,B1),
    append(A1,[','|B1],Temp),
    append(Temp,[')'],Tail).
processing(A*B,['*','('|Tail]):-
    processing(A,A1),processing(B,B1),
    append(A1,[','|B1],Temp),
    append(Temp,[')'],Tail).
processing(A/B,['/','('|Tail]):-
    processing(A,A1),processing(B,B1),
    append(A1,[','|B1],Temp),
    append(Temp,[')'],Tail).

