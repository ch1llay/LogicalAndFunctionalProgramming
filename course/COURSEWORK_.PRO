
translate(B):-write("enter excpression\n"), 
    read(A),
    processing(A,List),
    atom_chars(B,List).

processing(A,L):-
    atomic(A),
    atom_chars(A,L).
processing(A+B,['+',' ', Tail]):-
    processing(A,A1),processing(B,B1), 
    append(A1,[' '|B1],Tail).



