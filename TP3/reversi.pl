:- module(reversi,[vizinho/3, direcao/4, validar/3, jogar/4]).
:- use_module(auxiliar).


colunas(8).
linhas(8).
nextPlayer(o, x).
nextPlayer(x, o).

vizinho((L,C), Adversario, Board):- 
    direcao(_, (L,C), LV, CV),
    pos((LV,CV), Board, Adversario),
    !.

dentroBoard((L,C)):-
        linhas(MaxLinhas),
        colunas(MaxColunas),
        L > 0, L =< MaxLinhas ,
        C > 0 , L =<MaxColunas.

jogar((L, C), Board, NewBoard, Jogador):-
    jogadaValida((L,C), Jogador, Board),
    substituir(Board, L, C, Jogador, NewBoard),!.

jogadaValida((L,C), Jogador, Board):-
    jogadasPossiveis(Jogador,Board,ListaJogadasValidas),
    member((L,C),ListaJogadasValidas).

jogadasPossiveis(Jogador,Board,ListaJogadas):-
    nextPlayer(Jogador,Adversario),
    findall(
            (L,C),
            (   
                pos((L,C),Board,0),
                vizinho((L,C),Adversario,Board),
                validar((L,C),Jogador,Board)
            ),
            ListaJogadas
            ).

validar(Pos, Jogador, Board):-
    dentroBoard(Pos),
    nextPlayer(Jogador, Adversario),
    vizinho(Pos,Adversario,Board),
    validarDirecoes(Pos,Board,Jogador,Adversario,_),!.

validarDirecoes((L,C), Board, Jogador,Adversario,Direcao):-
    direcao(Direcao,(L,C),LinhaIndex,ColunaIndex),
    validarDirecoesAux((LinhaIndex,ColunaIndex), Board, Jogador,Adversario,Direcao,1). %%verificar esta direcção onde tem um vinho adversario

validarDirecoesAux((L,C),Board,Jogador,_,_,Index):-
    Index>1,
    pos((L,C),Board,Jogador)
    ,!.
validarDirecoesAux((L,C),Board,_,_,_,Index):-
    Index>1,
    pos((L,C),Board,0)
    ,!,fail.
validarDirecoesAux((L,C),Board,Jogador,Adversario,Direcao,Index):-
    pos((L,C),Board,Adversario),
    direcao(Direcao, (L,C), LV, CV),
    Index2 is Index+1,
    validarDirecoesAux((LV,CV),Board,Jogador,Adversario,Direcao,Index2),!.


%Direcoes
direcao(cima, (L,C), LSaida, C) :- coordenadas(LSaida, L).
direcao(baixo, (L,C), LSaida, C) :- coordenadas(L, LSaida).

direcao(esquerda, (L,C), L, CSaida) :- coordenadas(CSaida, C).
direcao(direita, (L,C), L, CSaida) :- coordenadas(C, CSaida).

direcao(cima_esquerda, (L,C), LSaida, CSaida) :- coordenadas(CSaida, C), coordenadas(LSaida, L).
direcao(cima_direita, (L,C), LSaida, CSaida) :- coordenadas(C, CSaida), coordenadas(LSaida, L).
direcao(baixo_esquerda, (L,C), LSaida, CSaida) :- coordenadas(CSaida, C), coordenadas(L, LSaida).
direcao(baixo_direita, (L,C), LSaida, CSaida) :- coordenadas(C, CSaida), coordenadas(L, LSaida).



