:- module(alphabeta, [alphabeta/3,profundidade/1]).
:- use_module(auxiliar).
:- use_module(reversi).
:- use_module(alphabeta).
%minMax com cortes alfa beta
profundidade(2).

alpha_limit(-1000).
beta_limit(1000).

utility([Jogador,State,Board],Val):-
    decide(Jogador,Board,State),
    nextPlayer(Jogador,Adversario),
    contarPecas(Jogador, Board, JogadorPecas),
    contarPecas(Adversario, Board, AdversarioPecas),
    Val is JogadorPecas - AdversarioPecas.

% alphabeta(Pos, BestNextPos, Val)
% Pos is a position, Val is its alphabeta value.
% Best move from Pos leads to position BestNextPos.
alphabeta(Pos, BestNextPos, Val) :-
    profundidade(Profundidade),
    alpha_limit(Alpha),
    beta_limit(Beta),
    alphabeta(Pos,Alpha,Beta,BestNextPos, Val,Profundidade).

alphabeta(Pos, Alfa, Beta, BestNextPos, Val, Profundidade) :-
    Profundidade > 0,
    ProfundidadeAux is Profundidade - 1,
    jogadasComHeuristica(Pos,ListaJogadasHeuristica),
    bounded_best(ListaJogadasHeuristica,Alfa,Beta,BestNextPos,Val,ProfundidadeAux).

alphabeta(Pos,_,_,_,Val,_):-
    utility(Pos,Val).

bounded_best([Pos|PosList], Alpha, Beta, GoodPos, GoodVal,Profundidade) :-
    alphabeta(Pos, Alpha, Beta, _, Val,Profundidade),
    good_enough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,Profundidade).

good_enough([],_,_,Pos,Val,Pos,Val,_):- !.

good_enough(_,Alpha,Beta,Pos,Val,Pos,Val,_):-
	min_to_move(Pos), Val > Beta,!
	;
	max_to_move(Pos), Val < Alpha, !.

good_enough(PostList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,Profundidade):-
	newbounds(Alpha,Beta,Pos,Val,NewAlpha,NewBeta),
	bounded_best(PostList,NewAlpha,NewBeta,Pos1,Val1,Profundidade),
	betterOf(Pos,Val,Pos1,Val1,GoodPos,GoodVal).


newbounds(Alpha,Beta,Pos,Val,Val,Beta):-
	min_to_move(Pos), Val > Alpha, !.
newbounds(Alpha,Beta,Pos,Val,Alpha,Val):-
	max_to_move(Pos), Val < Beta, !.
newbounds(Alpha,Beta,_,_,Alpha,Beta).


betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0
