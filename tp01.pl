:-dynamic(elemento/1).
:-dynamic(custo/2).
:-dynamic(componente/2).
%1) Carregue para memória a BC definida no ficheiro bicicletas.txt, que contém informação sobre a
%   especificação dos diferentes componentes de uma bicicleta.
carregar_ficheiro:-consult("bicicletas.pl").

%2)gerar_elemento().

gerar_elemento:-
                findall(X,(componente(X,_,_);componente(_,X,_)),LR),
                sort(LR, LR2),
                gerar_elemento(LR2).     


gerar_elemento([]).

gerar_elemento([H|T]):-
                assertz(elemento(H)),
                gerar_elemento(T).
                   
%3)Escreva o predicado produto_final(Elemento) que permite pesquisar elementos que não entram
%  na composição de outros.

produto_final(X):- 
                elemento(X),
                \+ componente(_,X,_).