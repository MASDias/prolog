% 1.a) mediaInteiros(Lista,R,Q).
contarElementos([],0).
contarElementos([_|T],N):-contarElementos(T,N1), N is N1+1.
somar([],0).
somar([H|T],S):-number(H), somar(T, S1), S is S1+H.
mediaInteiros([],_):-write("A lista está vazia!"),!,fail.
mediaInteiros(L,Q):-contarElementos(L,N), somar(L,R), Q is R/N.

% 1.b)menorValor(Lista, Elemento).
menorValor([E], E).
menorValor([H,N|T],E):-
    H =< N, 
    menorValor([H|T],E).

menorValor([H,N|T],E):-
    H > N, 
    menorValor([N|T],E).

% Outra Solução para 1.b):
menorValor2([H],H).
menorValor2([H|T], M):-menorValor2(T, MT), (H<MT, M=H,!;M=MT).

% 1.c)contarParesImpares(Lista, Pares, Impares).
contarParesImpares([], 0, 0).
contarParesImpares([H|T], P, I):-
    ((H mod 2) =:= 0,!, 
    contarParesImpares(T, P1, I2), 
    P is P1+1,
    I is I2).
contarParesImpares([H|T], P, I):-
    ((H mod 2) \== 0,!, 
    contarParesImpares(T, P2, I1), 
    I is I1+1,
    P is P2).

% 1.d)verificarRepetidos(Lista).
member(X,[X|_]).
member(X,[_|T]):-member(X,T).
verificarRepetidos([H|T]):-member(H,T),!.
verificarRepetidos([_|T]):-verificarRepetidos(T).

% 1.e)menorNaFrente(Lista, ListaResultado).
%Apagar todos os elementos repetidos de uma lista
apagar(_,[],[]).
apagar(X,[X|T],LR):-apagar(X,T,LR), !.
apagar(X,[H|T],[H|LR]):-apagar(X,T,LR).
menorNaFrente(L, LR):-menorValor(L, E),
                     apagar(E, L, LR2),
                     append([E|[]], LR2, LR),
                     !.

% 1.f)concatenarListas(Lista1, Lista2, ListaResultado).
concatenarListas(L1, L2, LR):-append(L1, L2, LR).

% 1.g)flatten(Lista, ListaResultado).
flatten([], []):-!.
flatten([H|T], LR):-!,
                flatten(H, NL), 
                flatten(T, NL2),
                append(NL, NL2, LR).
flatten(L, [L]).

%flatten2([], []):-!.
%flatten2([H|T],[L]):-H\==[], T=:=[], L=[H].
%flatten2([H|T], LR):-flatten2(H, LR1), flatten2(T, LR2), append(LR1,LR2,LR).

% 1.h)removeFirstOccurence(Lista, Elemento, ListaResultado).
removeFirstOccurence([],_,[]).
removeFirstOccurence([H|T], E, LR):-H =:= E, LR = T.
removeFirstOccurence([H|T], E, LR):-removeFirstOccurence(T, E, R2),!, append([H], R2, LR).

% 1.i)removeAllOccurences(Elemento, Lista, ListaResultado).
removeAllOccurences(E, L, LR):-apagar(E, L, LR).

% 1.j)replace(Lista, Elemento1, Elemento2, ListaResultado).
replace([], _, _, []).
replace([E|T], E, E2, [E2|T2]):-replace(T, E, E2, T2), !.
replace([H|T], E, E2, [H|T2]):- H \== E, replace(T, E, E2, T2).

% 1.k)insertElement(Lista, Elemento, Indice, ListaResultado).
insertElement(L, E, I, LR):-insertElement(L, E, I, 0, LR).
insertElement([], _, _, _, []).
insertElement([H|T], E, I, C, [H|T2]):- I \== C,
                                      C2 is C+1,
                                      insertElement(T, E, I, C2, T2),
                                      !.
insertElement(L, E, I, C, [E|T2]):- insertElement(L, E, I, C+1, T2),
                                    !.

insertElement2([H|T], E, I, [H|T2]):- I \== 0, 
                                    I2 is I-1, 
                                    insertElement2(T, E, I2, T2), 
                                    !.
insertElement2(L, E, 0, [E|L]).

% 1.l)inverter(Lista, ListaResultado).
inverter(L, LR):- reverse(L,LR).

% 1.m)uniao(Lista1, Lista2, ListaResultado).
uniao(L1, L2, LR):- union(L1, L2, LR).

% 1.n)interseccao(Lista1, Lista2, ListaResultado).
interseccao(L1, L2, LR):- intersection(L1, L2, LR).

% 1.o)diferencaListas(Lista1, Lista2, ListaResultado).
diferencaListas(L1, L2, LR):- intersection(L1, L2, LI), 
                            union(L1, L2, LU), 
                            diferencaListas2(LU, LI, LR).

diferencaListas2([],_,[]).

diferencaListas2([H|T],LI,[H|LR]):-
    \+ member(H,LI),
    !,
    diferencaListas2(T,LI,LR).

diferencaListas2([_|T],LI,LR):-
    diferencaListas2(T,LI,LR),
    !.

/*
dif(L1,L2,LR):- 
    setof(X,L1^L2^(member(X,L1);member(X,L2),LU), 
    setof(Y,L1^L2^(member(Y,L1),member(Y,L2),LI),
    findall(Z,(member(Z,LU),\+ member(Z,LI)),LR).
*/