bestfriends(X, Y) :-
    knowing(X, Y), knowing(Y, X).

knowing(X, Y) :-
    friend(X, Y).
    
knowing(X, Y) :-
    friend(Y, X).

friend(tom, jerry).
friend(X, X).
unknown(X, jerry).