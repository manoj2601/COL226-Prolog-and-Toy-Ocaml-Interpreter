likes(tom, jerry).
likes(jerry, tom).

loves(X, Y) :-
    likes(X, Y), likes(Y, X).