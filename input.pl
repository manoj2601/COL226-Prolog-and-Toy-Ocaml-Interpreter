bestfriends(X, Y) :-
    friends(X, Y), friends(Y, X).

friends(X, Y) :-
    known(X, Y).
    
known(tom, jerry).
known(jerry, tom).