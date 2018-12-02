:- module(auxiliar,[contarPecas/3, readPos/1, pos/3, coordenadas/2, inBoard/2, substituir/5, nextPlayer/2]).
:- use_module(reversi).

readPos(V):-
    read(V),
    V = (L,C).

contarPecas(Player,Board,TotalPecas):-
    flatten(Board, Lista),
    contarPecasLista(Player,Lista,TotalPecas,0).


contarPecasLista(_,[],T,T).

contarPecasLista(Player,[H|T],TotalPecas,Contador):-
    (
        H = Player -> Contador1 is Contador + 1
        ;
        Contador1 is Contador
    ),
    contarPecasLista(Player,T,TotalPecas,Contador1).
    

pos((L,C), Board, V):-
    nth1(L, Board, Linha),
    nth1(C, Linha, V).

nextPlayer(o, x).
nextPlayer(x, o).

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
