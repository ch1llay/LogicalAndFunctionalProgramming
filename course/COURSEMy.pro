domains
    clist = string*
    stack = string*

predicates
    operator(string, integer)
    isOperand(string)
    isOpenParenthesis(string)
    isClouseParenthesis(string)
    isParenthesis(string)

    changeParenthesis(string, string)
    push(stack, string, stack)
    pop(stack, string)
    readList(clist)
    reverse(clist, clist, clist)
    addToList(clist, string, clist)
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
     

    addToList([],El, [El]).
    addToList([H|T],El, [H|T1]):-addToList(T,El,T1).


      writeList([H|T]):-write(H),nl, !, writeList(T).
      writeList([]).

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
        addToList(Res, HIn, NewRes), !,
        convert(Tin, Stack, NewRes, Out).
    