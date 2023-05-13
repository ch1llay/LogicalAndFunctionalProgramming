%trace

predicates

operator(char)
convert(string, string, string, string)
literal(char)
check_result(string, string)
remove_space(string, string, string)
isSpace(char)
copy(string, string)


clauses

copy(S, S).

isSpace(C) :- C = ' '.
remove_space(In, Res, Out) :-
    frontchar(In, C, Rest),
    isSpace(C),
    remove_space(Rest,Res, OUT), !.

remove_space(In, Res, Out) :-
    frontchar(In, C, Rest),
    str_char(StrC, C),
    concat(StrC, Res, ResOut),
    remove_space(Rest,ResOut, Out), !.
    
 remove_space(_, Res, Out) :-
    copy(Res, Out), !.

operator('+').
operator('-').
operator('*').
operator('/').

literal(Char):- Char >= '0', Char <= '9'.
literal(Char):- Char >= 'a', Char <= 'z'.
literal(Char):- Char >= 'A', Char <= 'Z'.
literal(Char):- write("Incorrect char: "), write(Char), fail. 

check_result(S, E) :- str_len(S, In), str_len(E, Out), In = Out, !.
check_result(_, _) :- write("Incorrect format of input"), fail.

convert(In, Ops, Literals, OUT) :-
  frontchar(In, C, Rest),
  literal(C),
  str_char(StrC, C),
  concat(StrC, Literals, LiteralsRes),
  convert(Rest, Ops, LiteralsRes, OUT).

convert(In, Ops, Literals, OUT) :-
    frontchar(In, C, Rest),
    operator(C),
    str_char(StrC, C),
    concat(Ops, StrC, OpsRes),
    convert(Rest, OpsRes, Literals, OUT).

convert(_,  Ops, Literals, OUT) :-
    concat(Ops, Literals, OUT).

goal
	write("Enter infix form"), nl, readln(In), remove_space(In,"", A), convert(A, "", "",Res), check_result(A, Res), write("Prefix form:"), nl, write(Res).
