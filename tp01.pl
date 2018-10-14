:-dynamic(elemento/1).
:-dynamic(custo/2).
:-dynamic(componente/2).
%1) Carregue para memória a BC definida no ficheiro bicicletas.txt, que contém informação sobre a
%   especificação dos diferentes componentes de uma bicicleta.
load_file:-consult("bicicletas.pl").

%2)gerar_elemento().
gerar_elemento:-
                (componente(X,_,_);componente(_,X,_)),
                \+ elemento(X), 
                assert(elemento(X)), 
                fail;true.
                   

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


%5)Escreva o predicado produto_intermedio(Elemento) que permite pesquisar elementos que
%   entram na composição de outros e que são formados por outros elementos.
produto_intermedio(X):-
                elemento(X),
                \+ produto_base(X),
                \+ produto_final(X).


%6)Escreva o predicado nivel(ElementoX,ElementoY,Nivel) que permite determinar a
%   profundidade que o ElementoY está na árvore de produto relativamente ao ElementoX,
%   considere que a raiz está no nível zero.
nivel(X,Y,N):-diferenca_niveis(Y,X,0,N).

diferenca_niveis(Y,Y,N,N).

diferenca_niveis(Y,W,C,N):-
                            Y\==W,
                            componente(W,X,_),
                            C1 is C+1,
                            diferenca_niveis(Y,X,C1,N).

%7) Escreva   o   predicadodois_mais_profundos(ElementoRaiz,(EMax1,Prof1),(EMax2,Prof2))que  permite  
%   determinar  os  dois  elementos,  e  respectivas  profundidades,  que  estão  a  maior profundidade 
%   na árvore de produto ElementoRaiz.
dois_mais_profundos(ElementoRaiz,(EMax1,Prof1),(EMax2,Prof2)):-
                            findall((N,E),(nivel(ElementoRaiz,E,N)),L),
                            sort(L,L2),
                            append(_,[(Prof1,EMax1),(Prof2,EMax2)],L2),
                            !.


%8) Escreva o predicado reg_custo(Elemento,Custo)que permite criar um facto (dinâmico) com o custo  de  um  componente,  no  caso de  
%   já  existir  um  custo  para  esse  componente  deve  ser Assim, a  chamada  do  predicado reg_custo(pedal,  32)
%   deve  dar  origem  àcriação  do facto custo(pedal, 32) ou à sua atualização
reg_custo(E,C):- retract(custo(E,C)),fail.

reg_custo(E,C):- asserta(custo(E,C)).


%9) Escreva o predicado calc_custo(Elemento,Valor,LElemSemCusto) que permite calcular o
%   custo de um Elemento em função das quantidades e custos dos subprodutos que integram a
%   árvore do referido elemento; os subprodutos que não tenham custo definido devem ser
%   coleccionados na lista LElemSemCusto. 
calc_custo(E,V,L):-findall((X,Q),componente(E,X,Q),L2),
                   calc_custo_peca(L2,V,L).

calc_custo_peca([],0,[]).

calc_custo_peca([(X,Q)|T],V,L):-
                                custo(X,C),
                                !,
                                P is (C*Q),                             
                                calc_custo_peca(T,V2,L),
                                V is V2 + P. 

calc_custo_peca([(X,Q)|T],V,L):-
                                \+ custo(X,_),
                                calc_custo(X,V2,L2),
                                calc_custo_peca(T,V3,L3),
                                V is (V2*Q) + V3,
                                append([X],L2,L4),
                                append(L3,L4,L).
                                
                
%10)Escreva o predicado lista_materiais(Elemento,Qtd) que permite listar os produtos base e
%   respectivas quantidades e custos envolvidos na produção de uma Qtd de Elemento por ordem
%   decrescente de custo.
lista_materiais(E,Q):-lista_materiais(E,Q,L),
                    sort(L,L2),
                    reverse(L2,L3),
                    write(L3).

lista_materiais(E,Q,L):-findall((X,QX),componente(E,X,QX),L2),
                        encontrar_materiais(L2,Q,L).

encontrar_materiais([],0,[]).

encontrar_materiais([(X,Q2)|T],Q,L):-
                                    (produto_base(X),
                                    custo(X,C)),
                                    Q3 is (Q*Q2),
                                    encontrar_materiais(T,Q,L2),                                   
                                    append([(C,X,Q3)],L,L2)).

encontrar_materiais([(X,Q2)|T],Q,L):-
                                    (produto_base(X),
                                    \+custo(X,_)),
                                    Q3 is (Q*Q2),
                                    encontrar_materiais(T,Q,L2),                                   
                                    append([(0,X,Q3)],L,L2)).

encontrar_materiais([(X,_)|T],Q,L):-
                                    \+produto_base(X),
                                    lista_materiais(X,Q,L2),
                                    encontrar_materiais(T,Q,L3),
                                    append(L2,L3,L).
                                    


    
    