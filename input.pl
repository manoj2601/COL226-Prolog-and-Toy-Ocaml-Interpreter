loves(X, Y) :-
    likes(X, Y), likes(Y, X).

likes(A, B) :-
    friend(A, B).
    
likes(C, D) :-
    friend(D, C).

friend(tom, jerry).