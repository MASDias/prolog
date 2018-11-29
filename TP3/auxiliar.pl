:- module(auxiliar,[readPos/1, pos/3, jogar/4, coordenadas/2, inBoard/2]).

readPos(V):-
    read(V),
    V = (L,C).

pos((L, C), Board, V):-
    nth1(L, Board, Linha),
    nth1(C, Linha, V).

jogar((L, C), Board, NewBoard, E):-
    pos((L,C), Board, 0),
    %validar((L,C), Board, E)
    substituir(Board, L, C, E, NewBoard).

substituir(Board, L, C, E, NewBoard):-
    nth1(L, Board, List),
    selectInsert(C, List, E, NewLine),
    selectInsert(L, Board, NewLine, NewBoard).

selectInsert(N, I, V, O):-
    nth1(N,I,_,T),
    nth1(N,O,V,T).


coordenadas(1,2).
coordenadas(2,3).
coordenadas(3,4).
coordenadas(4,5).
coordenadas(5,6).
coordenadas(6,7).
coordenadas(7,8).

inBoard(8,9) :-!.
inBoard(A,B) :- coordenadas(A,B).
