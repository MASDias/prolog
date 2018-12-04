:- use_module(alphabeta).
% :- use_module(tictactoe).
:- use_module(auxiliar).
:- use_module(reversi).
:- dynamic(ai/1).
% bestMove(+Pos, -NextPos)
% Compute the best Next Position from Position Pos
% with minimax or alpha-beta algorithm.
% bestMove(Pos, NextPos) :-
%     minimax(Pos, NextPos, _).

% play
% Start the game.
play :-
    nl,
    write('===================='), nl,
    write('=      Reversi     ='), nl,
    write('===================='), nl, nl,
    write('Rem : x starts the game'), nl,
    chooseAI,
    playerMark.

chooseAI:-
    repeat,
    nl,
    retractall(ai/1),
    write('Decide what algorithm to use:'),nl,
    % write('1] Minimax'),nl,
    write('1] alpha-beta'),nl,
    read(AI),nl,
    (AI == 1,
    assert(ai(alfabeta))
    % ;
    % AI == 2,
    % assert(ai(minimax))
    ).

% playAskColor
% Ask the color for the human player and start the game with it.
playerMark:-
	  repeat,
	  nl, write('Symbol for human player ? (x or o)'), nl,
	  read(Player), nl,
	  (Player == o; Player == x),
          EmptyBoard = [[0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 'o', 'x', 0, 0, 0],
                        [0, 0, 0, 'x', 'o', 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0, 0]],
    write('  -------------------------------'), nl,
	  drawBoard(EmptyBoard), nl,
          play([x, play, EmptyBoard], Player).



% play(+Position, +HumanPlayer)
% If next player to play in position is equal to HumanPlayer -> Human must play
% Ask to human what to do.
play([Player, play, Board], Player) :- !,
    nl, write('Next move ?'), nl,
    readPos(Pos),            % Ask human where to play
    (
      humanMove([Player, play, Board], [NextPlayer, State, NextBoard], Pos), !,
      drawBoard(NextBoard),
      (
        State = win, !,                             % If Player win -> stop
        nl, write('End of game : '),
        write(Player), write(' win !'), nl, nl
        ;
        State = draw, !,                            % If draw -> stop
        nl, write('End of game : '),
        write(' draw !'), nl, nl
        ;
        play([NextPlayer, State, NextBoard], Player) % Else -> continue the game
      )
      ;
      write('-> Bad Move !'), nl,                % If humanMove fail -> bad move
      play([Player, play, Board], Player)        % Ask again
    ).


% play([Player, play, Board], Player2) :- !,
%     nl, write('Next move ?'), nl,
%     readPos(Pos),% Ask human where to play
%     (
%       humanMove([Player, play, Board], [NextPlayer, State, NextBoard], Pos), !,
%       drawBoard(NextBoard),
%       (
%         State = win, !,                             % If Player win -> stop
%         nl, write('End of game : '),
%         write(Player), write(' win !'), nl, nl
%         ;
%         State = draw, !,                            % If draw -> stop
%         nl, write('End of game : '),
%         write(' draw !'), nl, nl
%         ;
%         play([NextPlayer, play, NextBoard], Player2) % Else -> continue the game
%       )
%       ;
%       write('-> Bad Move !'), nl,                % If humanMove fail -> bad move
%       play([Player, play, Board], Player)        % Ask again
%     ).

play([Player, State, Board], HumanPlayer) :-
    nl, write('Computer play : '), nl, nl,
    (% Compute the best move
        % hasMoves(Player,Board),
        bestMove([Player, State, Board], [NextPlayer, NextState, BestSuccBoard]),
        % showBoard(BestSuccBoard),
        drawBoard(BestSuccBoard),
        (
          decide(Player,NextState,BestSuccBoard)
          ;
          % Else -> continue the game
          play([NextPlayer, play, BestSuccBoard], HumanPlayer)
        ),
        nextPlayer(Player,OtherPlayer),
        play([OtherPlayer, play, Board], HumanPlayer)
    )
    .

bestMove([Player,State,Board],NextPos):-
    % ai(AI),
    Pos=[Player,State,Board],
    (
        % AI==minimax,
        % minimax(Pos,NextPos,_,Depth)
        % ;
        alphabeta(Pos,NextPos,_)
    )
    .

% nextPlayer(X1, X2)
% True if X2 is the next player to play after X1.


% When human play

humanMove([Player, play, Board], [Adversario, State, NextBoard], Pos) :-
    nextPlayer(Player, Adversario),
    jogar(Pos, Board, NextBoard, Player), nl, 
    decide(Player,NextBoard,State).

% decide(X,Board,win):-
%     winPos(X,Board),!.
% decide(X,Board,draw):-
%     drawPos(X,Board),!.
% decide(_,_,play).

% decide(Jogador, Board, State) :-
%     contarPecas(0, Board, 0),
%     nextPlayer(Jogador, Adversario),
%     contarPecas(Jogador, Board, JogadorPecas),
%     contarPecas(Adversario, Board, AdversarioPecas),
%     (
%         JogadorPecas > AdversarioPecas, 
%         State = win
%         ; 
%         JogadorPecas =:= AdversarioPecas, 
%         State = draw
%     ),!.

% decide(_,_,play).


% set1(+Elem, +Pos, +List, -ResList).
% Set Elem at Position Pos in List => Result in ResList.
% Rem : counting starts at 1.
set1(1, E, [X|Ls], [E|Ls]) :- !, X = 0.

set1(P, E, [X|Ls], [X|L2s]) :-
    number(P),
    P1 is P - 1,
    set1(P1, E, Ls, L2s).


drawBoard([]):-!.

drawBoard([H|T]):-show(H),
                  drawBoard(T).

% show(+Board)
% Show the board to current output.
show([X1, X2, X3, X4, X5, X6, X7, X8]) :-
    write(' | '), show2(X1),
    write(' | '), show2(X2),
    write(' | '), show2(X3),
    write(' | '), show2(X4),
    write(' | '), show2(X5),
    write(' | '), show2(X6),
    write(' | '), show2(X7),
    write(' | '), show2(X8),
    write(' | '),nl,
    write('  -------------------------------'), nl.



% show2(+Term)
% Write the term to current outupt
% Replace 0 by ' '.
show2(X) :-
    X = 0, !,
    write(' ').

show2(X) :-
    write(X).
