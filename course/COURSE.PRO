DOMAINS
	list=string*
PREDICATES
	readlist(list)
	operator(string)
	delete_space(list,string,list)
	divide(list,list,list)
	append(list, list, list)
	len(list,integer)
CLAUSES
	readlist([H|T]):-readln(H),!,readlist(T).
	readlist([]).
	operator("+").
	operator("-").
	operator("*").
	operator("/").
	delete_space([],_,[]):-!.
	delete_space([Elem|T],Elem,ResT):-
		delete_space(T,Elem,ResT),!.
	delete_space([H|T],Elem,[H|ResT]):-
		delete_space(T,Elem, ResT).
	divide([],[],[]):-!.
	divide([H|T],[HO|TO],Numbers):-
		operator(S), S=H,HO=H,
		divide(T,TO,Numbers).
	divide([H|T],Operators,[HN|TN]):-
		operator(S), S<>H,str_int(H,N),
		HN=H,
		divide(T,Operators,TN).
	divide([H|T],Operators,Numbers):-
		operator(S), S<>H, 
		divide(T,Operators,Numbers).
	
	len([H|T],L):-len(T,TL),
		L=TL+1.
	len([],0):-!.
	
	append([], L, L).
	append([El|T], L, [El|L]).
GOAL
	write("Enter your expression"), nl, 
	List=["1", "+", "1"], delete_space(List,"", Res),
	write("Your expression:"), nl, write(Res),
	nl, divide(Res,Operations,Numbers),
	write("Your operations:"), nl,
	write(Operations), nl, write("Your numbers"),
	nl, write(Numbers),nl, 
	len(Numbers,NN), len(Operations,NO),
	append(Operations, Numbers, Result),
	write("Result:"),nl,write(Result).
