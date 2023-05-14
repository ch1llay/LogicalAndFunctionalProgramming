domains
    clist = string*
    stack = string*

predicates
    operator(string, integer)
    isLiteral(string)
    isParenthesis(string)
    changeParenthesis(string, string)
    push(stack, string, stack)
    pop(stack, string)
    readList(clist)
    reverse(clist, clist, clist)
    len(clist, integer)
    changeParenthesisList(clist, clist)
    prepareList(clist, clist)
    writeList(clist)
    %convert(clist, stack, clist, string)
    

clauses
    operator("+", 1).
    operator("-", 1).
    operator("*", 2).
    operator("/", 2).

    isLiteral(C):-
        not(operator(C, _)),
        C >= "0", C <= "9".

     isLiteral(C):-
        not(operator(C, _)),
        C >= "a" , C <= "z".
    isLiteral(C):-
        not(operator(C, _)),
        C >= "A", C <= "Z".   

    isParenthesis(C):-
        C = "("; C=")".
    
    push(S, El, [El|S]).
    pop([El|T], El).

    readList([H|T]):-
        write("el: "),
        readln(H), !,
        readList(T).
    readList([]).

    reverse([],Z,Z).
    reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

    len([H|T],L):-len(T,TL),
		L=TL+1.
	len([],0):-!.

    changeParenthesis("(", ")").
    changeParenthesis(")", "(").

    changeParenthesisList([HIn|TIn], [HOut|TOut]) :-
        isParenthesis(HIn),
        changeParenthesis(HIn, HOut), !,
        changeParenthesisList(Tin, TOut).

      changeParenthesisList([], []).
      changeParenthesisList([HIn|TIn], [HOut|TOut]) :-
        HOut = HIn, !,
        changeParenthesisList(Tin, TOut).
      
      prepareList(In, Out) :-
      	changeParenthesisList(In, Temp),
      	reverse(Temp, Out, []).       
     
        
      writeList([H|T]):-write(H),nl, !, writeList(T).
      writeList([]).

    %convert([HIn|Tin], [HS|TS], [HR|TR], Out):-


    
