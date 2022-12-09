trace
domains 
   list=integer* 
predicates 
   create(list) 
   reverse(list,list,list) 
   nondeterm polyndrom(list,list) 
   nondeterm append(list,list,list)
   makezero(list)
   getindexsublist(list, list, list) 
clauses 
        create([H|T]):-write("elem--> "),readint(H),!,create(T).
        create([]). 
        
        getindexsublist(SubL, L, IndxL) :- 
    
        reverse([H|T],S,L):- reverse(T,[H|S],L). 
        reverse([],L,L). 
        
        polyndrom(L,L1):- append(L2,_,L),L2=[_,_|_],
        append(_,L1,L2),L1=[_,_|_], not(reverse(L1,[],L1)). 
        
        append([],L,L).   
        append([H|T],L,[H|L1]):- append(T,L,L1). 
goal 
%   clearwindow, 
   L = [1, 2, 3], 
   write("List: ",L),nl, 
   polyndrom(L,L1), 
   write("Palindrom: ",L1),nl,fail,
   append(L,L1,L2), 
   write("The List ",L2),nl.