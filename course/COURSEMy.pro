domains
    clist = char*
    stack = char*

predicates
    operator(char, integer)
    isLiteral(char)
    isParenthesis(char)
    push(stack, char, stack)
    pop(stack, char)
    convert(clist, stack, clist, string)

clauses
    operator('+', 1).
    operator('-', 1).
    operator('*', 2).
    operator('/', 2).

    isLiteral(C):-
        not(operator(C, _)),
        C >= '0', C <= '9'.

     isLiteral(C):-
        not(operator(C, _)),
        C >= 'a' , C <= 'z'.
    isLiteral(C):-
        not(operator(C, _)),
        C >= 'A', C <= 'Z'.   

    isParenthesis(C):-
        C = '('; C=')'.
    
    push(S, El, [El|S]).
    pop([El|T], El).
