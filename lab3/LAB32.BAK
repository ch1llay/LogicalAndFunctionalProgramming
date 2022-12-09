trace

domains
    list=integer*
predicates
    sum_list(list, integer)
clauses
    sum_list([],0).

    sum_list([H|T], Sum) :-

    sum_list(T,Sum1),

    Sum = H + Sum1.

  
 goal
 	sum_list([1, 2, 2], Sum), nl,
 	write(Sum).

 	