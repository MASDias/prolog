
% -----------------------------------------------------------------------
% Trabalho pr�tico: factos de cidades com localiza��o baseada em
% latitude e longitude e predicado auxiliar para calcular a dist�ncia
% entre quaisquer duas destas cidades.
% ------------------------------------------------------------------------

%city(name,latitude,longitude)
city(brussels,50.8462807,4.3547273).
city(tirana,41.33165,19.8318).
city(andorra,42.5075025,1.5218033).
city(vienna,48.2092062,16.3727778).
city(minsk,53.905117,27.5611845).
city(sarajevo,43.85643,18.41342).
city(sofia,42.6976246,23.3222924).
city(zagreb,45.8150053,15.9785014).
city(nicosia,35.167604,33.373621).
city(prague,50.0878114,14.4204598).
city(copenhagen,55.6762944,12.5681157).
city(london,51.5001524,-0.1262362).
city(tallinn,59.4388619,24.7544715).
city(helsinki,60.1698791,24.9384078).
city(paris,48.8566667,2.3509871).
city(marseille,43.296386,5.369954).
city(tbilisi,41.709981,44.792998).
city(berlin,52.5234051,13.4113999).
city(athens,37.97918,23.716647).
city(budapest,47.4984056,19.0407578).
city(reykjavik,64.135338,-21.89521).
city(dublin,53.344104,-6.2674937).
city(rome,41.8954656,12.4823243).
city(pristina,42.672421,21.164539).
city(riga,56.9465346,24.1048525).
city(vaduz,47.1410409,9.5214458).
city(vilnius,54.6893865,25.2800243).
city(luxembourg,49.815273,6.129583).
city(skopje,42.003812,21.452246).
city(valletta,35.904171,14.518907).
city(chisinau,47.026859,28.841551).
city(monaco,43.750298,7.412841).
city(podgorica,42.442575,19.268646).
city(amsterdam,52.3738007,4.8909347).
city(belfast,54.5972686,-5.9301088).
city(oslo,59.9138204,10.7387413).
city(warsaw,52.2296756,21.0122287).
city(lisbon,38.7071631,-9.135517).
city(bucharest,44.430481,26.12298).
city(moscow,55.755786,37.617633).
city(san_marino,43.94236,12.457777).
city(edinburgh,55.9501755,-3.1875359).
city(belgrade,44.802416,20.465601).
city(bratislava,48.1483765,17.1073105).
city(ljubljana,46.0514263,14.5059655).
city(madrid,40.4166909,-3.7003454).
city(stockholm,59.3327881,18.0644881).
city(bern,46.9479986,7.4481481).
city(kiev,50.440951,30.5271814).
city(cardiff,51.4813069,-3.1804979).

%  dist_cities(brussels,prague,D).
%  D = 716837.
dist_cities(C1,C2,Dist):-
    city(C1,Lat1,Lon1),
    city(C2,Lat2,Lon2),
    distance(Lat1,Lon1,Lat2,Lon2,Dist).

degrees2radians(Deg,Rad):-
	Rad is Deg*0.0174532925.

% distance(latitude_first_point,longitude_first_point,latitude_second_point,longitude_second_point,distance
% in meters)
distance(Lat1, Lon1, Lat2, Lon2, Dis2):-
	degrees2radians(Lat1,Psi1),
	degrees2radians(Lat2,Psi2),
	DifLat is Lat2-Lat1,
	DifLon is Lon2-Lon1,
	degrees2radians(DifLat,DeltaPsi),
	degrees2radians(DifLon,DeltaLambda),
	A is sin(DeltaPsi/2)*sin(DeltaPsi/2)+ cos(Psi1)*cos(Psi2)*sin(DeltaLambda/2)*sin(DeltaLambda/2),
	C is 2*atan2(sqrt(A),sqrt(1-A)),
	Dis1 is 6371000*C,
	Dis2 is round(Dis1).

% distance(50.8462807,4.3547273,50.0878114,14.4204598,D).
% Online: http://www.movable-type.co.uk/scripts/latlong.html
%


%1) tsp1(Origem, Destino, Caminho, Custo).
/*bnb1(Orig, Dest, Cam, Custo):- 
	bnb2(Dest, [(0,[Orig])], Cam, Custo).

bnb2(Dest, [(Custo,[Dest|T])|_], Cam, Custo):- 
	reverse([Dest|T], Cam).

bnb2(Dest, [(Ca,LA)|Outros], Cam, Custo):-
	LA = [Act|_],
	findall((CaX,[X|LA]),(Dest \== Act, dist_cities(Act, X, CustoX),\+ member(X,LA), CaX is CustoX + Ca), Novos),
	append(Outros, Novos, Todos),
	sort(Todos, TodosOrd),
	bnb2(Dest, TodosOrd, Cam, Custo).
*/

% Given three colinear points p, q, r, the function checks if
% point q lies on line segment 'pr'
% onSegment(P, Q, R)
onSegment((PX,PY), (QX,QY), (RX,RY)):-
    QX =< max(PX,RX),
    QX >= min(PX,RX),
    QY =< max(PY,RY),
    QY >= min(PY,RY).

 
% To find orientation of ordered triplet (p, q, r).
% The function returns following values
% 0 --> p, q and r are colinear
% 1 --> Clockwise
% 2 --> Counterclockwise

orientation((PX,PY), (QX,QY), (RX,RY), Orientation):-
	Val is (QY - PY) * (RX - QX) - (QX - PX) * (RY - QY),	
	(
		Val == 0, !, Orientation is 0;
		Val >0, !, Orientation is 1;
		Orientation is 2
	).

orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4):-
    orientation(P1, Q1, P2,O1),
    orientation(P1, Q1, Q2,O2),
    orientation(P2, Q2, P1,O3),
    orientation(P2, Q2, Q1,O4).
 
 
 
% The main function that returns true if line segment 'p1q1'
% and 'p2q2' intersect.
doIntersect(P1,Q1,P2,Q2):-
    % Find the four orientations needed for general and
    % special cases
	orientation4cases(P1,Q1,P2,Q2,O1,O2,O3,O4),
	(	
    % General case
    O1 \== O2 , O3 \== O4,!;

    % Special Cases
    % p1, q1 and p2 are colinear and p2 lies on segment p1q1
    O1 == 0, onSegment(P1, P2, Q1),!;
 
    % p1, q1 and p2 are colinear and q2 lies on segment p1q1
    O2 == 0, onSegment(P1, Q2, Q1),!;
 
    % p2, q2 and p1 are colinear and p1 lies on segment p2q2
    O3 == 0, onSegment(P2, P1, Q2),!;
 
     % p2, q2 and q1 are colinear and q1 lies on segment p2q2
    O4 == 0, onSegment(P2, Q1, Q2),!
    ).    


%----------------------------------------------------------------------------------------------------
% rGraph(Origin,UnorderedListOfEdges,OrderedListOfEdges)
%
% Examples:
% ---------
% ?- rGraph(a,[[a,b],[b,c],[c,d],[d,f],[f,e],[e,a]],R).
%
% ?- rGraph(brussels,[[vienna, sarajevo], [sarajevo, tirana],[tirana,sofia], [sofia, minsk], [andorra,brussels],[brussels,minsk],[vienna,andorra]],R).
%
%
rGraph(Orig,[(Orig,Z)|R],R2):-!,
	reorderGraph([(Orig,Z)|R],R2).
rGraph(Orig,R,R3):-
	member([Orig,X],R),!,
	delete(R,[Orig,X],R2),
	reorderGraph([[Orig,X]|R2],R3).
rGraph(Orig,R,R3):-
	member([X,Orig],R),
	delete(R,[X,Orig],R2),
	reorderGraph([[Orig,X]|R2],R3).


reorderGraph([],[]).

reorderGraph([[X,Y],[Y,Z]|R],[[X,Y]|R1]):-
	reorderGraph([[Y,Z]|R],R1).

reorderGraph([[X,Y],[Z,W]|R],[[X,Y]|R2]):-
	Y\=Z,
	reorderGraph2(Y,[[Z,W]|R],R2).

reorderGraph2(_,[],[]).
reorderGraph2(Y,R1,[[Y,Z]|R2]):-
	member([Y,Z],R1),!,
	delete(R1,[Y,Z],R11),
	reorderGraph2(Z,R11,R2).
reorderGraph2(Y,R1,[[Y,Z]|R2]):-
	member([Z,Y],R1),
	delete(R1,[Z,Y],R11),
	reorderGraph2(Z,R11,R2).



/*
calcular_total_cidades(N):- findall(Nome, city(Nome,_,_), Cidades),
							length(Cidades, N).

tsp1(Orig, Cam, Custo):-
						calcular_total_cidades(NCidades),
						findall((CustoX,Caminho),
							(bnb1(Orig,_,Caminho,CustoX), length(Caminho,Nx), Nx =:=NCidades),
							BNBPaths),
						calcular_caminhos_com_origem(Orig,BNBPaths,NewPath),
						sort(NewPath,Ordenados),
						melhor_caminho(Ordenados,Cam,Custo).

melhor_caminho([(Custo,Caminho)|_],Caminho,Custo).

calcular_caminhos_com_origem(_,[],[]). 

calcular_caminhos_com_origem(Origem,[(Custo,Caminho)|T],NovoCam):- 
						last(Caminho,Ultimo),
						dist_cities(Ultimo,Origem,UltimaDistancia),
						Resultado is Custo + UltimaDistancia,
						calcular_caminhos_com_origem(Origem,T,Caminho2),
						append(Caminho,Origem,CaminhoTemporario),
						append([(Resultado,CaminhoTemporario)],Caminho2,NovoCam).
*/
%--------------------------------BEAST MODE-------------------------------------
tsp(Ini, Cam, Custo):- 
					findall((CustoX,CamX),(tsp_2(Ini,CamX,CustoX)),LSol),
					sort(LSol, [(Custo,Cam)|_]).

tsp_2(Ini, Cam, Custo):-
					findall(C, city(C,_,_),LC), 
					select(Ini,LC,LR),
					tsp(Ini,[Ini],LR,Cam,Custo).

tsp(Ini,[H|T],[],Cam,CustoX):-
					dist_cities(Ini,H,CustoX),
					reverse([H|T],Cam).

tsp(Ini,[H|T],LV,Cam,Custo):-
					select(X,LV,LVR),
					tsp(Ini,[X,H|T],LVR,Cam,CustoP),
					dist_cities(X,H,Cx),
					Custo is CustoP + Cx.



%2) Implemente	 o	 predicado	 tsp2,	 utilizando	 uma	 heurística	 greedy,	 a	 heurística	 do	
%	vizinho	mais	próximo (ideia	base:	próxima	cidade	a	ser	visitada	é	a	mais	próxima	que	
%	ainda	não	foi	visitada.						
%tsp1(Orig, Cam, Custo)
tsp2(Ini, Cam, Custo):- 
	findall((CustoX,CamX),(tsp_3(Ini,CamX,CustoX)),LSol),
	sort(LSol, [(Custo,CamAux)|_]),
	append(CamAux, [Ini], Cam).


tsp_3(Ini, Cam, Custo):-
	findall(C, city(C,_,_),LC), 
	select(Ini,LC,LR),
	tspAux(Ini,[Ini],LR,Cam,Custo).

tspAux(Ini,[H|T],[],Cam,CustoX):-
	dist_cities(Ini,H,CustoX),
	reverse([H|T],Cam).

tspAux(Ini,[H|T],LV,Cam,Custo):-
	findall((CustoC,Nc),(member(Nc,LV),dist_cities(H,Nc,CustoC)),LR2),
	sort(LR2,[(Cx,Melhor)|_]),
	select(Melhor,LV,LVR),
	tspAux(Ini,[Melhor,H|T],LVR,Cam,CustoP),
	Custo is CustoP + Cx.



% removerCruzamentos(Ini, Cam, Custo,Novo):- 
% 								tsp2(Ini, Cam, Custo),
% 								refazerCaminho(Ini,Cam,Res),
% 								reverse(Res,Novo).

% refazerCaminho(_,[],[]).
% refazerCaminho(Ini,[Ini],[]).
% refazerCaminho(Ini,[A,B|T],Novo):-
% 						S = [A,B],
% 						refazerCaminho(Ini,[B|T],Aux),
% 						append(Aux, [S], Novo).

tsp3(Ini, Cam, Custo):- 
						tsp2(Ini, L, _),
						refazerCaminho(Ini,L,Res),
						reverse(Res,Novo),
						removerCruzamentos(Ini,Novo,LR),	
						calcularCusto(LR,Custo),
						desfazerCaminho(LR,Cam).

calcularCusto([],0).
calcularCusto([[A,B]|T],Custo):-
						dist_cities(A,B,CX),
						calcularCusto(T,Custo2),
						Custo is Custo2+CX.


desfazerCaminho([[A,B]|T],Result):-
						CIni = [A,B],
						desfazerCaminho2(T,Result2),
						reverse(Result2,Aux),
						append(CIni,Aux,Result).

desfazerCaminho2([],_).
desfazerCaminho2([[_,A]|T],Result):-
			desfazerCaminho2(T,Result2),
			append(Result2,[A],Result).
			
refazerCaminho(_,[],[]).

refazerCaminho(Ini,[Ini],_).

refazerCaminho(Ini,[A,B|T],Novo):-
					S = [A,B],
					refazerCaminho(Ini,[B|T],Aux),
					append(Aux, [S], Novo).

removerCruzamentos(Ini,[[A,B],[C,D]|T],LR):-
					opt2(Ini,[[A,B],[C,D]|T],[[A,B],[C,D]|T],[[C,D]|T],LAux),
					(
						(
							[[A,B],[C,D]|T] \== LAux,	
							removerCruzamentos(Ini,LAux,LR)
						)
						;
						LR = LAux	
					).

intersecao(A,B,C,D):-	
					B\==C,
					city(A,XA,YA),
					city(B,XB,YB),
					city(C,XC,YC),
					city(D,XD,YD),
					doIntersect((XA,YA),(XB,YB),(XC,YC),(XD,YD)).

opt2(_,Result,[],_,Result).

opt2(Ini,Original,[_,A|T],[],Cam):-
    opt2(Ini,Original,[A|T],T,Cam).


opt2(Ini,Original,[[A,B]|T1],[[C,D]|T],Result):-
				(
					intersecao(A,B,C,D),
					Novo = [A,C],
					Novo2 = [B,D],
					select([A,B], Original, Original2),
					select([C,D], Original2, Original3),
					append(Original3, [Novo], NovoCaminho),
					append(NovoCaminho, [Novo2], NovoCaminho2),
					rGraph(Ini,NovoCaminho2,Result)
				)
				;
				(
					opt2(Ini,Original,[[A,B]|T1],T,Result) 
				).




%%-----------------------------------------------------------------------------------------------------------------
%%--------------------------------------------ITERACAO 4 - GENETICOS--------------------------------------------%%
custoCaminho(L,Custo):-
    L=[Ini|T],
    custo(L,Custo1),
    append(_,[Ultimo],T),
    dist_cities(Ultimo,Ini,CustoVoltaOrigem),
    Custo is Custo1+CustoVoltaOrigem.

custo([_],0):-!.

custo([A,B|T], Custo):-
	custo([B|T],C2),
	dist_cities(A,B,C1),
	Custo is C1 + C2.
:-dynamic cities/1.
:-dynamic best_path/1.
:-dynamic best_pop/1.
% parameteriza��o
geracoes(50).
populacao(200).
prob_cruzamento(0.5).
prob_mutacao(0.6).
%cities(6).


gerarNumeroCidades:-
	retractall(cities(_)),
    findall(Cidade,city(Cidade,_,_),ListaCidades),
    length(ListaCidades,N),
    assert(cities(N)).

gera:-
	gerarNumeroCidades,
	(
		\+ best_pop(_),
		gerar_populacao(Pop),
		avalia_populacao(Pop,PopAv), % Avaliacao do custo dos individuos(lista de possiveis caminhos) da populacao
		ordena_populacao(PopAv,PopOrd), % Apos a avaliacao da populacao faz uma ordenacao à mesma
		geracoes(NG),
		gera_geracao(NG,PopOrd)
	)
	;
	(
		best_pop(BestPop),
		geracoes(NG),
		gera_geracao(NG,BestPop)
	).
	
gerar_populacao(Pop):-
	populacao(TamanhoPopulacao),
	cities(NumCidades),
	findall(Cidade,city(Cidade,_,_), ListaCidades),
	gera_populacao(TamanhoPopulacao,ListaCidades,NumCidades,Pop).

gera_populacao(0,_,_,[]):-!.

gera_populacao(TamanhoPopulacao,ListaCidades,NumCidades,[Ind|Resto]):-
	TamanhoPopulacao1 is TamanhoPopulacao-1,
	gera_populacao(TamanhoPopulacao1,ListaCidades,NumCidades,Resto), %% Resto e a lista com todos os individuos (lista de listas de cidades)
	gera_individuo(ListaCidades,NumCidades,Ind),
	not(member(Ind,Resto)). %%Ind e um individuo (uma lista de cidades)

gera_populacao(TamanhoPopulacao,ListaCidades,NumCidades,L):-
	gera_populacao(TamanhoPopulacao,ListaCidades,NumCidades,L).

gera_individuo([G],1,[G]):-!.

gera_individuo(ListaCidades,NumCidades,[G|Resto]):-
	NumTemp is NumCidades + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaCidades,G,NovaLista),
	NumT1 is NumCidades-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).

retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).

avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

avalia(Cidades,Custo):-
	custoCaminho(Cidades,Custo).

ordena_populacao(PopAv,PopOrd):-
	bSort(PopAv,PopOrd).

bSort([X],[X]):-!.
bSort([X|Xs],Ys):-
	bSort(Xs,Zs),
	btroca([X|Zs],Ys).

btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).

atualizar_populacao(_).
atualizar_populacao(Pop):-
	\+ best_pop(_),
	assertz(best_pop(Pop)).
atualizar_populacao(Pop):-
	retractall(best_pop(_)),
	assertz(best_pop(Pop)).

gera_geracao(0,Pop):-!,
	best_path(Melhor),
	write('Fim...'),nl, write('Melhor Resultado => '),write(Melhor), nl,
	atualizar_populacao(Pop).

gera_geracao(G,Pop):-
	cruzamento(Pop,NovaPopulacao1),
	mutacao(NovaPopulacao1,NovaPopulacao),
	avalia_populacao(NovaPopulacao,NovaPopulacaoAvaliada),
	ordena_populacao(NovaPopulacaoAvaliada,NovaPopulacaoOrd),
	populacao(NumPopulacao),
	melhoresResultadosPopulacao(NumPopulacao,NovaPopulacaoOrd,NovaPopulacaoElitista),
	NovaPopulacaoElitista = [_*Valor|_],
	Geracoes is G-1,
	write('Geracao '),write(Geracoes),write('  '), write(Valor), nl,
	guardar_melhor_resultado(Valor),
	gera_geracao(Geracoes,NovaPopulacaoElitista).


guardar_melhor_resultado(Valor):-
	\+ best_path(_), %se nao existir um melhor resultado
	assertz(best_path(Valor)). %% entao estabelecer novo resultado

guardar_melhor_resultado(Valor):-
	best_path(Anterior), %% veriificar queal o resultado que existe no best path
	Anterior > Valor, % so o novo  resultado for melhor
	retractall(best_path(_)), % remover o anterior
	assertz(best_path(Valor)). % adicionar esse novo resultado

guardar_melhor_resultado(_).

melhoresResultadosPopulacao(NumPopulacao,Pop,Res):-
	findall(Elemento,(nth1(Index,Pop,Elemento), Index =< NumPopulacao),Res).
%para cada elemento Index(Começa em 1) da lista Pop filtra a populacao mediante o numero da populacao
%Obtendo assim apenas as melhores populacoes 


cruzamento([],[]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2,Ind1,Ind2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(ProbCruzamento),
	random(0.0,1.0,Pc),
	(
		(
		Pc =< ProbCruzamento,!,
        cruzar(Ind1,Ind2,P1,P2,NInd1),
		cruzar(Ind2,Ind1,P1,P2,NInd2)
		)
	;
		(
		NInd1=Ind1,NInd2=Ind2
		)
	),
	cruzamento(Resto,Resto1).

gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	cities(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	cities(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	cities(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	cities(NumeroCidades),
	R is NumeroCidades-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).

eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).
