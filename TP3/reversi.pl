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
    propagarJogada((L,C), Jogador, Board, NewBoard).

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

propagarJogada(Pos, Jogador, Board, NewBoard):- %%necessario mudar as pecas em todas as direcoes depois de jogar (se possivel)
    nextPlayer(Jogador,Adversario),
    mudarPecas(Pos,Jogador,Adversario,Board,TempBoard1,cima),
    mudarPecas(Pos,Jogador,Adversario,TempBoard1,TempBoard2,baixo),
    mudarPecas(Pos,Jogador,Adversario,TempBoard2,TempBoard3,esquerda),
    mudarPecas(Pos,Jogador,Adversario,TempBoard3,TempBoard4,direita),
    mudarPecas(Pos,Jogador,Adversario,TempBoard4,TempBoard5,cima_direita),
    mudarPecas(Pos,Jogador,Adversario,TempBoard5,TempBoard6,cima_esquerda),
    mudarPecas(Pos,Jogador,Adversario,TempBoard6,TempBoard7,baixo_direita),
    mudarPecas(Pos,Jogador,Adversario,TempBoard7,NewBoard,baixo_esquerda).

mudarPecas((L,C),Jogador,Adversario,Board,NewBoard,Direcao):-
    substituir(Board, L, C, Jogador, NewBoard1),
    (
            direcao(Direcao, (L,C), LinhaNova, ColunaNova),!,
            mudarPecasAux((LinhaNova,ColunaNova),Jogador,Adversario,Direcao,NewBoard1,NewBoard,1)
            ;
            NewBoard=NewBoard1
    ).

mudarPecasAux((L,C),Jogador,Adversario,Direcao,Board,NewBoard,Index):-
    pos((L,C),Board,Adversario),
    substituir(Board, L, C, Jogador, NewBoard1),
    direcao(Direcao,(L,C),LinhaNova,ColunaNova),
    Index2 is Index+1,
    mudarPecasAux((LinhaNova,ColunaNova),Jogador,Adversario,Direcao,NewBoard1,NewBoard,Index2).
mudarPecasAux((L,C),Jogador,_,_,Board,Board,I):-
    I>1,
    pos((L,C),Board,Jogador)
    ,!. %se onde esta a peca for a mesma parar
mudarPecasAux((L,C),_,_,_,Board,_,I):-
    I>1,
    pos((L,C),Board,0)
    ,!,fail. %se a proxima peca for zero parar

mudarPecasAux(_,_,_,_,Board,Board,_). %atribuir a o board como resultado

validar(Pos, Jogador, Board):-
    dentroBoard(Pos),
    nextPlayer(Jogador, Adversario),
    vizinho(Pos,Adversario,Board),
    validarDirecoes(Pos,Board,Jogador,Adversario,_),!.


%Validar em todas as direcoes se tem uma peca ao longo da direcao que tem a mesma que esta a ser jogada%
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



