flight("bursa","istanbul",3).
flight("istanbul","bursa",3).
flight("istanbul","ankara",4).
flight("ankara","istanbul",4).
flight("ankara","konya",2).
flight("konya","ankara",2).
flight("konya","sakarya",2).
flight("sakarya","konya",2).



route(X,Y,C) :- flight(X,Y,C).
route(X,Y,C) :- route(X,Y,C,[]).
route(X,Y,C,Liste) :- flight(X,Z,A)  , route(Z,Y,B,[Z|Liste]), not(X=Y), C is A + B.
route(X,Y,C,X).
member(X, [X|_]).       
member(X, [_|Tail]) :- member(X, Tail).


