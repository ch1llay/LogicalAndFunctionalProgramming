domains
predicates
    man(symbol)
    woman(symbol)
    husband(symbol, symbol)
    wife(symbol, symbol)
    married(symbol, symbol)
    isMarried(symbol, symbol)
    parent(symbol, symbol)
    father(symbol, symbol)
    mother(symbol, symbol)
    brother(symbol, symbol)
    sister(symbol, symbol)
    son(symbol, symbol)
    daughter(symbol, symbol)
    grandfather(symbol, symbol)
    grandmother(symbol, symbol)
    grandson(symbol, symbol)
    granddaughter(symbol, symbol)



clauses
    man(nikolay).
    man(petr).
    man(alexey).
    man(maxim).
    man(fedor).
    woman(elena).
    woman(vera).
    woman(natalia).
    woman(anastasia).
   
    married(vera, nikolay).  
   
    married(elena, petr).

    married(natalia, alexey). 
    
     parent(elena, alexey). 
     parent(petr, alexey).
    parent(vera, natalia). parent(nikolay, natalia).
    parent(alexey, maxim). parent(natalia, fedor). parent(alexey, fedor). parent(natalia, anastasia). parent(alexey, anastasia).
    parent(natalia, maxim).

    father(F, C) :- man(F), parent(F, C).
    mother(F, C) :- woman(F), parent(F, C).
    isMarried(X, Y) :- married(X, Y); married(Y, X).
    wife(W, M) :- isMarried(W, M), woman(W).
    husband(M, W) :- isMarried(M, W), man(M).
    son(S, P) :- man(S), parent(P, S).
    daughter(S, P) :- woman(S), parent(P, S), S<>P.
    grandmother(G, GC) :- woman(G), parent(G, C), parent(C, GC).
    grandfather(G, GC) :- man(G), parent(G, C), parent(C, GC).
    grandson(S, G) :- man(S), grandmother(GM, S); grandfather(S, G).
    granddaughter(S, G) :- man(S), grandmother(GM, S); grandfather(S, G).
    brother(B, C) :- man(B), father(P, B), father(P, C), B<>C.
    sister(S, C) :- woman(S), father(P, S), father(P, C), S<>C.



    
    
%goal
 % write("Parent "), readstring(P), parent(P, C), write(C, ", "), fail.
    


