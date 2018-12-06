:- module(minimax, [minimax/3]).
:- use_module(auxiliar).
:- use_module(reversi).

profundidade(2).

utility([Jogador,State,Board],Val):-
    decide(Jogador,Board,State),
    nextPlayer(Jogador,Adversario),
    contarPecas(Jogador, Board, JogadorPecas),
    contarPecas(Adversario, Board, AdversarioPecas),
    Val is JogadorPecas - AdversarioPecas.

minimax(Pos, BestNextPos, Val) :-
    profundidade(Profundidade),
    minimax(Pos, BestNextPos, Val, Profundidade),!.

minimax(Pos, BestNextPos, Val, Profundidade) :-
    Profundidade > 0,
    ProfundidadeAux is Profundidade - 1,
    jogadasComHeuristica(Pos,ListaJogadasHeuristica),
    best(ListaJogadasHeuristica, BestNextPos, Val, ProfundidadeAux), !.

minimax(Pos, _, Val,_) :-
    utility(Pos, Val).

best([Pos], Pos, Val, N) :-
    minimax(Pos, _, Val, N), !.

best([Pos1 | PosList], BestPos, BestVal, N) :-
    minimax(Pos1, _, Val1, N),
    best(PosList, Pos2, Val2, N),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal),!.

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

