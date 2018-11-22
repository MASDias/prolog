:- module(auxiliar,[readPos/1,pos/3]).

readPos(V):-
    read(V),
    V = (L,C).

pos((L, C), Board, V):-
    nth1(L, Board, Linha),
    nth1(C, Linha, V).

