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
    %convert(clist, clist, clist, string)
    

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

    len([H|T],L):-len(T,TL), L=TL+1.
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
        not(isOpenParenthesis), !,
        pop(Stack, El, NewStack),
        addToList(ResIn, El, Res),
        popStackToOpenParenthesis(Tin, NewStack, Res, OUTStack, OUTRes).
    
    popStackToOpenParenthesis([HIn|_], Stack, ResIn, OUTStack, OUTRes):-
        isOpenParenthesis(HIn), !,
        pop(Stack, _, NewStack),
        popStackToOpenParenthesis([], NewStack, ResIn, OUTStack, OUTRes).

    popStackToOpenParenthesis(_, _, _, _, _) :- write("error format").
    
    popStackToOpenParenthesis([], Stack, ResIn, Stack, ResIn).

    workWithOperator(Stack, El, Res, NewStack, NewRes):-
        len(Stack) > 0,
        pop(Stack, El, _),
        isOpenParenthesis(El),
        push(Stack, El, TempNewStack),
        workWithOperator(TempNewStack, El, Res, NewStack, NewStack)
    
    workWithOperator(Stack, El, Res, NewStack, NewRes):-
        len(Stack) > 0,
        pop(Stack, ElFromStack, Temp, TempNewStack),
        operator(ElFromStack, PrS),
        operator(El, PrE),
        PrS > PrE,
        addToList(Res, ElFromStack, TempNewRes),
        workWithOperator(TempNewStack, El, TempNewRes, NewStack, NewRes).

    workWithOperator([], El, Res, NewStack, NewRes):-
        push(Stack, El, NewStack),
        NewRes = Res, !.


    workWithOperator(Stack, El, Res, NewStack, NewRes):-
        len(Stack) > 0, !.
    
    
    convert([HIn|Tin], Stack, Res, Out):-
        isOpenParenthesis(HIn),
        push(Stack, HIn, NewStack), !,
        convert(Tin, NewStack, Res, Out).
    
    convert([HIn|Tin], Stack, Res, Out):-
        isOperand(HIn),
        addToList(Res, HIn, NewRes), !,
        convert(Tin, Stack, NewRes, Out).

    convert([HIn|Tin], Stack, Res, Out):-
        isClouseParenthesis(HIn),
        len(Stack) > 0,
        popStackToOpenParenthesis([HIn|TIn], Stack, Res, NewStack, NewRes),
        convert(Tin, NewStack, NewRes, Out).
    
    convert([HIn|Tin], Stack, Res, Out):-
        isOperand(HIn),
        len(Stack) = 0,
        push(Stack, HIn, NewStack),
        convert(Tin, NewStack, Res, Out).
    
    convert([HIn|Tin], Stack, Res, Out):-
        isOperand(HIn),
        len(Stack) > 0,
        convert(Tin, NewStack, Res, Out).
    
    convert([], Stack, Res, Out):-
        insertList(Stack, Res, TempRes), !,
        reverse(Temp, Out).

    convert(_, _, _, _):-
        write("error, not correct format").

    
    