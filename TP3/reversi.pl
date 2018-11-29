:- module(reversi,[vizinho/3, direcao/4, validar/3]).
:- use_module(auxiliar).

vizinho((L,C), Adversario, Board):- 
    direcao(_, (L,C), LV, CV),
    pos((LV,CV), Board, Adversario),
    !.

% validar((L,C), Board, Player):-
%     validar((L,C), Board,_),
%     !.


validar((L,C), Board,Direcao):-
    pos((L,C), Board, Player),
    direcao(Direcao, (L,C), ),
    !.

%Direcoes
direcao(cima, (L,C), LSaida, C) :- coordenadas(LSaida, L).
direcao(baixo, (L,C), LSaida, C) :- coordenadas(L, LSaida).

direcao(esquerda, (L,C), L, CSaida) :- coordenadas(CSaida, C).
direcao(direita, (L,C), L, CSaida) :- coordenadas(C, CSaida).

direcao(cima_esquerda, (L,C), LSaida, CSaida) :- coordenadas(CSaida, C), coordenadas(LSaida, L).
direcao(cima_direita, (L,C), LSaida, CSaida) :- coordenadas(C, CSaida), coordenadas(LSaida, L).
direcao(baixo_esquerda, (L,C), LSaida, CSaida) :- coordenadas(CSaida, C), coordenadas(L, LSaida).
direcao(baixo_direita, (L,C), LSaida, CSaida) :- coordenadas(C, CSaida), coordenadas(L, LSaida).



