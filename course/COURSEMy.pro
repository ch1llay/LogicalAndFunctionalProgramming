trace
nowarnings

domains
    clist = string*

predicates
    operator(string, integer)
    isOperand(string)
    isOpenParenthesis(string)
    isClouseParenthesis(string)
    isParenthesis(string)

    changeParenthesis(string, string)
    push(clist, string, clist)
    pop(clist, string, clist)
    readList(clist)
    reverse(clist, clist, clist)
    addToList(clist, string, clist)
    len(clist, integer)
    changeParenthesisList(clist, clist)
    prepareListForConvert(clist, clist)
    insertList(clist, clist, clist)
    writeList(clist)
    popStackToOpenParenthesis(clist, clist, clist, clist, clist)
    workWithOperator(clist, string, clist, clist, clist )
    convert_exp(clist, clist, clist, clist)
    convert(clist, clist)
    main()
clauses
    operator("+", 1).
    operator("-", 1).
    operator("*", 2).
    operator("/", 2).

    isOperand(C):-
        not(operator(C, _)),
        C >= "0", C <= "9".

     isOperand(C):-
        not(operator(C, _)),
        C >= "a" , C <= "z".
    isOperand(C):-
        not(operator(C, _)),
        C >= "A", C <= "Z".   

    isOpenParenthesis(C):-
        C = "(".
    isClouseParenthesis(C):-
        C=")".
    isParenthesis(C):-
        isOpenParenthesis(C);isClouseParenthesis(C).
    
    push(S, El, [El|S]).
    pop([El|T], El, T).

    readList([H|T]):-
        write("el: "),
        readln(H), !,
        readList(T).
    readList([]).

    reverse([],Z,Z).
    reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

    len([], 0):-!.
    len([_Head|Tail], Length):-
        len(Tail, TailLength),
        Length = TailLength + 1.

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
      
      prepareListForConvert(In, Out) :-
      	changeParenthesisList(In, Temp),
      	reverse(Temp, Out, []).       
     

    addToList([],El, [El]).
    addToList([H|T],El, [H|T1]):-addToList(T,El,T1).

    insertList([], L, L).
    insertList([H|T], L1, L2) :-
        addToList(L1,H, L),
        insertList(T, L, L2).


      writeList([H|T]):-write(H),nl, !, writeList(T).
      writeList([]).

    popStackToOpenParenthesis([HIn|TIn], Stack, ResIn, OUTStack, OUTRes):-
        not(isOpenParenthesis(HIn)), !,
        pop(Stack, El, NewStack),
        addToList(ResIn, El, Res),
        popStackToOpenParenthesis(Tin, NewStack, Res, OUTStack, OUTRes).
    
    popStackToOpenParenthesis([HIn|_], Stack, ResIn, OUTStack, OUTRes):-
        isOpenParenthesis(HIn), !,
        pop(Stack, _, NewStack),
        popStackToOpenParenthesis([], NewStack, ResIn, OUTStack, OUTRes).

    popStackToOpenParenthesis(_, _, _, _, _) :- write("error format").
    
    popStackToOpenParenthesis([], Stack, ResIn, Stack, ResIn).

    workWithOperator([], El, Res, NewStack, NewRes):-
        push(Stack, El, NewStack),!,
        NewRes = Res.

    workWithOperator(Stack, El, Res, NewStack, NewRes):-
        len(Stack, L), L > 0,
        pop(Stack, El, _),
        isOpenParenthesis(El),
        push(Stack, El, TempNewStack),!,
        workWithOperator(TempNewStack, El, Res, NewRes, NewStack).
    
    workWithOperator(Stack, El, Res, NewStack, NewRes):-
        len(Stack, L), L > 0,
        pop(Stack, ElFromStack, TempNewStack),

        operator(ElFromStack, PrS),
        operator(El, PrE),
        PrS > PrE,
        addToList(Res, ElFromStack, TempNewRes),!,
        workWithOperator(TempNewStack, El, TempNewRes, NewStack, NewRes).

   
    workWithOperator(Stack, El, Res, NewStack, NewRes):-
        len(Stack, L), L > 0,
        push(Stack, El, NewStack),
        NewRes = Res,
 !.
    
    
    convert_exp([HIn|Tin], Stack, Res, Out):-
        isOpenParenthesis(HIn),
        push(Stack, HIn, NewStack), !,
        convert_exp(Tin, NewStack, Res, Out).
    
    convert_exp([HIn|Tin], Stack, Res, Out):-
        isOperand(HIn),
        addToList(Res, HIn, NewRes), !,
        convert_exp(Tin, Stack, NewRes, Out).

    convert_exp([HIn|Tin], Stack, Res, Out):-
        isClouseParenthesis(HIn),!,
        len(Stack, L), L > 0,

        popStackToOpenParenthesis([HIn|TIn], Stack, Res, NewStack, NewRes),
        convert_exp(Tin, NewStack, NewRes, Out).
    
    convert_exp([HIn|Tin], Stack, Res, Out):-
        operator(HIn,P),
        len(Stack, L), L = 0,
        push(Stack, HIn, NewStack), !,
        convert_exp(Tin, NewStack, Res, Out).
    
    convert_exp([HIn|Tin], Stack, Res, Out):-
        operator(HIn, _),
        len(Stack, L), L > 0,
	    workWithOperator(Stack, HIn, Res, NewStack, NewRes),
        convert_exp(Tin, NewStack, NewRes, Out).
    
    convert_exp([], Stack, Res, Out):-
        insertList(Stack, Res, Out), !.
        %reverse(Temp, Out, []).

    convert_exp(_, _, _, _):-
        write("error, not correct format").
    
    convert(In, Out):-
    	convert_exp(In, [], [], TempRes),!,
    	reverse(TempRes, Out, []), !.
    
    main():-
    	write("Write prefix expression "), nl, L = ["1", "*", "1", "+", "2", "*", "2"], prepareListForConvert(L, PL),nl, convert(PL, Out), write(Out), writeList(Out).
    	