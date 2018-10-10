:-dynamic(elemento/1).
:-dynamic(custo/2).
:-dynamic(componente/2).
%1) Carregue para memória a BC definida no ficheiro bicicletas.txt, que contém informação sobre a
%   especificação dos diferentes componentes de uma bicicleta.
carregar_ficheiro:-consult("bicicletas.pl").

%2)gerar_elemento().

novo:-
                componente(X,Y,_),
                (
                    (\+ elemento(X),assertz(elemento(X)));
                    (\+ elemento(Y),assertz(elemento(Y)))
                ),
                fail;true.

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

%4)Escreva o predicado produto_base(Elemento) que permite pesquisar elementos que apenas
%   entram na composição de outros. 
produto_base(X):-
                elemento(X),
                \+componente(X,_,_).

%Escreva o predicado produto_intermedio(Elemento) que permite pesquisar elementos que
%entram na composição de outros e que são formados por outros elementos

produto_intermedio(X):-
                elemento(X),
                \+ produto_base(X),
                \+ produto_final(X).


%8) Escreva o predicado reg_custo(Elemento,Custo)que permite criar um facto (dinâmico) com o custo  de  um  componente,  no  caso de  
% já  existir  um  custo  para  esse  componente  deve  ser Assim, a  chamada  do  predicado reg_custo(pedal,  32)
% deve  dar  origem  àcriação  do facto custo(pedal, 32) ou à sua atualização

reg_custo(E,C):- retract(custo(E,C)),fail.

reg_custo(E,C):- asserta(custo(E,C)).
