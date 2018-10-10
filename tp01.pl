:-dynamic(elemento/1).
:-dynamic(custo/2).
:-dynamic(componente/2).
%1) Carregue para memória a BC definida no ficheiro bicicletas.txt, que contém informação sobre a
%   especificação dos diferentes componentes de uma bicicleta.
carregar_ficheiro():-consult("biciletas.txt").

%2)gerar_elemento().
/*
gerar_elemento(LR):-setof(P,(X^componente(P,X,_)),LR1),
                    setof(Y,(L^componente(L,Y,_)),LR2),
                    append(LR1,LR2,LR).
                */