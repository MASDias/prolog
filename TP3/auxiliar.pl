:- module(auxiliar,[decide/3, min_to_move/1, max_to_move/1, jogadasComHeuristica/2, contarPecas/3, readPos/1, pos/3, coordenadas/2, inBoard/2, substituir/5, nextPlayer/2,decide/3]).
:- use_module(reversi).

nextPlayer(o, x).
nextPlayer(x, o).

readPos(V):-
    read(V).

contarPecas(Player,Board,TotalPecas):-
    flatten(Board, Lista),
    contarPecasLista(Player,Lista,TotalPecas,0).

contarPecasLista(_,[],TotalPecas,TotalPecas).

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

temJogadaPossivel(Jogador,Board):-
    pos(Pos,Board,0),
    validar(Pos, Jogador, Board). 

substituir(Board, L, C, E, NewBoard):-
    nth1(L, Board, List),
    selectInsert(C, List, E, NewLine),
    selectInsert(L, Board, NewLine, NewBoard).

selectInsert(N, I, V, O):-
    nth1(N,I,_,T),
    nth1(N,O,V,T).

%da as jogadas com os estados possiveis (ganhar, empatar, perder, play)
jogadasComHeuristica(Pos,ListaJogadas):-
    findall(Jogada, 
                jogadaHeuristica(Pos,Jogada),
            ListaJogadas).

jogadaHeuristica([Jogador, _, Board], [Adversario, State, NewBoard]):-
    nextPlayer(Jogador,Adversario),
    jogar(_,Board,NewBoard,Jogador),
    decide(Jogador,NewBoard,State).

ganhar(Jogador,Board,State):-
    nextPlayer(Jogador, Adversario),
    (
        contarPecas(0, Board, 0)
        ;
        \+ temJogadaPossivel(Jogador,Board),  
        \+ temJogadaPossivel(Adversario,Board)  
    ),
    contarPecas(Jogador, Board, JogadorPecas),
    contarPecas(Adversario, Board, AdversarioPecas),
    JogadorPecas > AdversarioPecas, 
    State = win.
    
empatar(Jogador,Board,State):-
    nextPlayer(Jogador, Adversario),
    (
        contarPecas(0, Board, 0)
        ;
        \+ temJogadaPossivel(Jogador,Board),  
        \+ temJogadaPossivel(Adversario,Board)  
    ),
    contarPecas(Jogador, Board, JogadorPecas),
    contarPecas(Adversario, Board, AdversarioPecas),
    JogadorPecas =:= AdversarioPecas, 
    State = draw.

perder(Jogador,Board,State):-
    nextPlayer(Jogador, Adversario),
    (
        contarPecas(0, Board, 0)
        ;
        \+ temJogadaPossivel(Jogador,Board),  
        \+ temJogadaPossivel(Adversario,Board)
    ),
    contarPecas(Jogador, Board, JogadorPecas),
    contarPecas(Adversario, Board, AdversarioPecas),
    JogadorPecas < AdversarioPecas, 
    State = lose.

decide(Jogador, Board, State) :-
    (
        % perder(Jogador,Board,State)
        % ;
        ganhar(Jogador,Board,State)
        ; 
        empatar(Jogador,Board,State)
    ),!.

decide(_,_,play).


% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([o, _, _]).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([x, _, _]).

coordenadas(1,2).
coordenadas(2,3).
coordenadas(3,4).
coordenadas(4,5).
coordenadas(5,6).
coordenadas(6,7).
coordenadas(7,8).

inBoard(8,9) :-!.
inBoard(A,B) :- coordenadas(A,B).
