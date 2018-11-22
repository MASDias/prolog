:- module(auxiliar,[readPos/1, pos/3, jogar/4]).

readPos(V):-
    read(V),
    V = (L,C).

pos((L, C), Board, V):-
    nth1(L, Board, Linha),
    nth1(C, Linha, V).

jogar((L, C), Board, NewBoard, E):-
    pos((L,C), Board, 0),
    substituir(Board, L, C, E, NewBoard).

substituir(Board, L, C, E, NewBoard):-
    nth1(L, Board, List),
    selectInsert(C, List, E, NewLine),
    selectInsert(L, Board, NewLine, NewBoard).

selectInsert(N, I, V, O):-
    nth1(N,I,_,T),
    nth1(N,O,V,T).