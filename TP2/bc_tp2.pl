
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
% city(sarajevo,43.85643,18.41342).
% city(sofia,42.6976246,23.3222924).
% city(zagreb,45.8150053,15.9785014).
% city(nicosia,35.167604,33.373621).
% city(prague,50.0878114,14.4204598).
% city(copenhagen,55.6762944,12.5681157).
% city(london,51.5001524,-0.1262362).
% city(tallinn,59.4388619,24.7544715).
% city(helsinki,60.1698791,24.9384078).
% city(paris,48.8566667,2.3509871).
% city(marseille,43.296386,5.369954).
% city(tbilisi,41.709981,44.792998).
% city(berlin,52.5234051,13.4113999).
% city(athens,37.97918,23.716647).
% city(budapest,47.4984056,19.0407578).
% city(reykjavik,64.135338,-21.89521).
% city(dublin,53.344104,-6.2674937).
% city(rome,41.8954656,12.4823243).
% city(pristina,42.672421,21.164539).
% city(riga,56.9465346,24.1048525).
% city(vaduz,47.1410409,9.5214458).
% city(vilnius,54.6893865,25.2800243).
% city(luxembourg,49.815273,6.129583).
% city(skopje,42.003812,21.452246).
% city(valletta,35.904171,14.518907).
% city(chisinau,47.026859,28.841551).
% city(monaco,43.750298,7.412841).
% city(podgorica,42.442575,19.268646).
% city(amsterdam,52.3738007,4.8909347).
% city(belfast,54.5972686,-5.9301088).
% city(oslo,59.9138204,10.7387413).
% city(warsaw,52.2296756,21.0122287).
% city(lisbon,38.7071631,-9.135517).
% city(bucharest,44.430481,26.12298).
% city(moscow,55.755786,37.617633).
% city(san_marino,43.94236,12.457777).
% city(edinburgh,55.9501755,-3.1875359).
% city(belgrade,44.802416,20.465601).
% city(bratislava,48.1483765,17.1073105).
% city(ljubljana,46.0514263,14.5059655).
% city(madrid,40.4166909,-3.7003454).
% city(stockholm,59.3327881,18.0644881).
% city(bern,46.9479986,7.4481481).
% city(kiev,50.440951,30.5271814).
% city(cardiff,51.4813069,-3.1804979).

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
						tsp2(Ini, Cam, Custo),
						refazerCaminho(Ini,Cam,Res),
						reverse(Res,Novo),
						removerCruzamentos(Ini,Novo,[],LR),
						Cam = LR.

refazerCaminho(_,[],[]).

refazerCaminho(Ini,[Ini],[]).

refazerCaminho(Ini,[A,B|T],Novo):-
						S = [A,B],
						refazerCaminho(Ini,[B|T],Aux),
						append(Aux, [S], Novo).

removerCruzamentos(_,[],Result,Result).

removerCruzamentos(Ini,[[A,B],[C,D]|T],L,LR):-
								opt2(Ini,[[A,B],[C,D]|T],[A,B],[[C,D]|T],Result),
								write('saddas'),
								rGraph(Ini,Result,LR),
								removerCruzamentos(Ini,[[C,D]|T], L, LR).

intersecao(A,B,C,D):-	
					B\==C,
					city(A,XA,YA),
					city(B,XB,YB),
					city(C,XC,YC),
					city(D,XD,YD),
					doIntersect((XA,YA),(XB,YB),(XC,YC),(XD,YD)).

custo(A,_,C,D,CustoAC,CustoAD):- 				
			dist_cities(A,C,CustoAC),		
			dist_cities(A,D,CustoAD).

% doIntersect(P1,Q1,P2,Q2):

opt2(_,[_],[_,_],[],[_]).

opt2(Ini,Original,[A,B],[[C,D]|T], Result):-
				(
					intersecao(A,B,C,D),
					custo(A,B,C,D,CustoAC,CustoAD),				
					(
						CustoAC > CustoAD,
						Novo = [A,D],
						Novo2 = [D,B]
						;
						Novo = [A,C],
						Novo2 = [C,B]
					),
					nl,
					write(A),
					nl,
					write(B),
					nl,
					write(C),
					nl,
					write(D),
					nl,
					write(Original),
					nl,
					select([A,B], Original, Original2),
					select([C,D], Original2, Original3),
					append(Original3, [Novo], NovoCaminho),
					append(NovoCaminho, [Novo2], NovoCaminho2),
					Result = NovoCaminho2,
					write(Novo),
					nl,
					write(Novo2),
					nl,
					nl,
					write(NovoCaminho2),
					nl,
					nl,
					opt2(Ini,NovoCaminho2,Novo,T,Result)

				)
				;
				(
					opt2(Ini,Original,[A,B],T,Result) 
				).