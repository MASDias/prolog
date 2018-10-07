% 3.a)
vizinho(X,Y):-fronteira(X,Y).
vizinho(X,Y):-fronteira(Y,X).

% 3.b)
contSemPaises(C):-continente(C),\+pais(_,C,_).

% 3.c)
semVizinhos(L):-pais(L,_,_),\+vizinho(L,_).

% 3.d)
chegoLaFacil(P1,P2):-vizinho(P1,P2),!;vizinho(P1,P3),vizinho(P2,P3).

% 4.a) pot(Base,Expoente,Resultado).
potPositiva(_,0,1).
potPositiva(B,E,R):-E>0, E1 is E-1, potPositiva(B, E1, R1), R is B*R1.
pot(B,E,R):-E<0, E1 is E*(-1), potPositiva(B, E1, R1), R is 1/R1.
pot(B,E,R):-E>=0, potPositiva(B,E,R).

% 4.b) factorial(X,R).
factorial(0,1):-!.
factorial(X,R):-X1 is X-1, factorial(X1,R1), R is X*R1.

% 4.c) somatorio(J,K,R).
somatorio(J,K,R):-J>K, R is 0.
somatorio(J,K,R):-J=<K, J1 is J+1, somatorio(J1, K, R1), R is R1+J.

% 4.d) divisaoInteira(Dividendo, Divisor, Resto, Quociente).
divisaoInteira(Dividendo, Divisor, R, Q):-Dividendo < Divisor, R is Dividendo, Q is 0, !.
divisaoInteira(Dividendo, Divisor, R, Q):-Dividendo == Divisor, R is 0, Q is 1, !.
divisaoInteira(Dividendo, Divisor, R, Q):-C is (Dividendo-Divisor), divisaoInteira(C, Divisor, R2, Q2), R is R2, Q is (Q2 + 1).

% 5.e) prime(P).
prime(2).
prime(3).
prime(P):-rPrime(2,P).
rPrime(2,P):-(P mod 2)\==0, rPrime(3,P).
rPrime(D,P):-D<P, (P mod D)\==0, D2 is D+2, rPrime(D2,P).
rPrime(D,P):-D>=P,!.

% 5.f) gcd(A,B,R).
gcd(A,0,R):-R is A,!.
gcd(A,B,R):-C is (A mod B), gcd(B, C, R2), R is R2.

% Continentes
continente(asia).
continente(europa).
continente(america).
continente(africa).
continente(oceania).
continente(teste).
% Fronteiras
fronteira(portugal, spain).
fronteira(france, germany).
fronteira(france,spain).
fronteira(germany, holanda).
fronteira(china, tailandia).
fronteira(spain, india).
fronteira(brazil, argentina).
fronteira(brazil, portugal).
fronteira(argentina, angola).
fronteira(angola, portugal).
% Paises
% Asia
pais(afghanistan, asia, 31).
pais(armenia, asia, 3).
pais(azerbaijan, asia, 8).
pais(bahrain, asia, 1).
pais(bangladesh, asia, 147).
pais(belarus, asia, 10).
pais(bhutan, asia, 2).
pais(brunei, asia, 0).
pais(burma, asia, 47).
pais(cambodia, asia, 14).
pais(china, asia, 1314).
pais(cyprus, asia, 1).
pais(easttimor, asia, 1).
pais(gazastrip, asia, 1).
pais(georgia, asia, 5).
pais(hongkong, asia, 7).
pais(india, asia, 1095).
pais(indonesia, asia, 245).
pais(iran, asia, 69).
pais(iraq, asia, 27).
pais(israel, asia, 6).
pais(japan, asia, 127).
pais(jordan, asia, 6).
pais(kazakhstan, asia, 15).
pais(koreanorth, asia, 23).
pais(koreasouth, asia, 49).
pais(kuwait, asia, 2).
pais(kyrgyzstan, asia, 5).
pais(laos, asia, 6).
pais(lebanon, asia, 4).
pais(macau, asia, 0).
pais(malaysia, asia, 24).
pais(maldives, asia, 0).
pais(moldova, asia, 4).
pais(mongolia, asia, 3).
pais(nepal, asia, 28).
pais(oman, asia, 3).
pais(pakistan, asia, 166).
pais(philippines, asia, 89).
pais(qatar, asia, 1).
pais(russia, asia, 143).
pais(saudiarabia, asia, 27).
pais(singapore, asia, 4).
pais(srilanka, asia, 20).
pais(syria, asia, 19).
pais(taiwan, asia, 23).
pais(tajikistan, asia, 7).
pais(thailand, asia, 65).
pais(turkey, asia, 70).
pais(turkmenistan, asia, 5).
pais(ukraine, asia, 47).
pais(unitedarabemirates, asia, 3).
pais(uzbekistan, asia, 27).
pais(vietnam, asia, 84).
pais(westbank, asia, 2).

% Africa
pais(algeria, africa, 33).
pais(angola, africa, 12).
pais(benin, africa, 8).
pais(botswana, africa, 2).
pais(burkinafaso, africa, 14).
pais(burundi, africa, 8).
pais(cameroon, africa, 17).
pais(capeverde, africa, 0).
pais(centralafricanrep, africa, 4).
pais(centralafricanrep, africa, 4).
pais(chad, africa, 10).
pais(comoros, africa, 1).
pais(congodemrep, africa, 63).
pais(congorepubofthe, africa, 4).
pais(cotedivoire, africa, 18).
pais(djibouti, africa, 0).
pais(egypt, africa, 79).
pais(equatorialguinea, africa, 1).
pais(eritrea, africa, 5).
pais(ethiopia, africa, 75).
pais(gabon, africa, 1).
pais(gambiathe, africa, 2).
pais(ghana, africa, 22).
pais(guinea, africa, 10).
pais(guinea-bissau, africa, 1).
pais(kenya, africa, 35).
pais(lesotho, africa, 2).
pais(liberia, africa, 3).
pais(libya, africa, 6).
pais(madagascar, africa, 19).
pais(malawi, africa, 13).
pais(mali, africa, 12).
pais(mauritania, africa, 3).
pais(mauritius, africa, 1).
pais(mayotte, africa, 0).
pais(morocco, africa, 33).
pais(mozambique, africa, 20).
pais(namibia, africa, 2).
pais(niger, africa, 13).
pais(nigeria, africa, 132).
pais(reunion, africa, 1).
pais(rwanda, africa, 9).
pais(sainthelena, africa, 0).
pais(saotomeprincipe, africa, 0).
pais(senegal, africa, 12).
pais(seychelles, africa, 0).
pais(sierraleone, africa, 6).
pais(somalia, africa, 9).
pais(southafrica, africa, 44).
pais(southafrica, africa, 44).
pais(sudan, africa, 41).
pais(swaziland, africa, 1).
pais(tanzania, africa, 37).
pais(togo, africa, 6).
pais(tunisia, africa, 10).
pais(uganda, africa, 28).
pais(westernsahara, africa, 0).
pais(zambia, africa, 12).
pais(zimbabwe, africa, 12).

% America
pais(anguilla, america, 0).
pais(antiguabarbuda, america, 0).
pais(argentina, america, 40).
pais(aruba, america, 0).
pais(bahamasthe, america, 0).
pais(barbados, america, 0).
pais(belize, america, 0).
pais(bermuda, america, 0).
pais(bolivia, america, 9).
pais(brazil, america, 188).
pais(britishvirginis, america, 0).
pais(canada, america, 33).
pais(caymanislands, america, 0).
pais(chile, america, 16).
pais(colombia, america, 44).
pais(costarica, america, 4).
pais(cuba, america, 11).
pais(dominica, america, 0).
pais(dominicanrepublic, america, 9).
pais(ecuador, america, 14).
pais(elsalvador, america, 7).
pais(frenchguiana, america, 0).
pais(greenland, america, 0).
pais(grenada, america, 0).
pais(guadeloupe, america, 0).
pais(guatemala, america, 12).
pais(guyana, america, 1).
pais(haiti, america, 8).
pais(honduras, america, 7).
pais(jamaica, america, 3).
pais(martinique, america, 0).
pais(mexico, america, 107).
pais(montserrat, america, 0).
pais(netherlandsantilles, america, 0).
pais(nicaragua, america, 6).
pais(panama, america, 3).
pais(paraguay, america, 7).
pais(peru, america, 28).
pais(puertorico, america, 4).
pais(saintkittsnevis, america, 0).
pais(saintlucia, america, 0).
pais(stpierremiquelon, america, 0).
pais(saintvincentandthegrenadines, america, 0).
pais(suriname, america, 0).
pais(trinidadtobago, america, 1).
pais(turkscaicosis, america, 0).
pais(unitedstates, america, 298).
pais(uruguay, america, 3).
pais(venezuela, america, 26).
pais(virginislands, america, 0).

% Europa
pais(albania, europa, 4).
pais(andorra, europa, 0).
pais(austria, europa, 8).
pais(belgium, europa, 10).
pais(bosniaherzegovina, europa, 4).
pais(bulgaria, europa, 7).
pais(croatia, europa, 4).
pais(czechrepublic, europa, 10).
pais(denmark, europa, 5).
pais(estonia, europa, 1).
pais(faroeislands, europa, 0).
pais(finland, europa, 5).
pais(france, europa, 61).
pais(germany, europa, 82).
pais(gibraltar, europa, 0).
pais(greece, europa, 11).
pais(guernsey, europa, 0).
pais(hungary, europa, 10).
pais(iceland, europa, 0).
pais(ireland, europa, 4).
pais(isleofman, europa, 0).
pais(italy, europa, 58).
pais(jersey, europa, 0).
pais(latvia, europa, 2).
pais(liechtenstein, europa, 0).
pais(lithuania, europa, 4).
pais(luxembourg, europa, 0).
pais(macedonia, europa, 2).
pais(malta, europa, 0).
pais(monaco, europa, 0).
pais(netherlands, europa, 16).
pais(norway, europa, 5).
pais(poland, europa, 39).
pais(portugal, europa, 11).
pais(romania, europa, 22).
pais(sanmarino, europa, 0).
pais(serbia, europa, 9).
pais(slovakia, europa, 5).
pais(slovenia, europa, 2).
pais(spain, europa, 40).
pais(sweden, europa, 9).
pais(switzerland, europa, 8).
pais(unitedkingdom, europa, 61).

% Oceania
/*
pais(americansamoa, oceania, 0).
pais(australia, oceania, 20).
pais(cookislands, oceania, 0).
pais(fiji, oceania, 1).
pais(frenchpolynesia, oceania, 0).
pais(guam, oceania, 0).
pais(kiribati, oceania, 0).
pais(marshallislands, oceania, 0).
pais(micronesiafedst, oceania, 0).
pais(nauru, oceania, 0).
pais(newcaledonia, oceania, 0).
pais(newzealand, oceania, 4).
pais(nmarianaislands, oceania, 0).
pais(palau, oceania, 0).
pais(papuanewguinea, oceania, 6).
pais(samoa, oceania, 0).
pais(solomonislands, oceania, 1).
pais(tonga, oceania, 0).
pais(tuvalu, oceania, 0).
pais(vanuatu, oceania, 0).
pais(wallisandfutuna, oceania, 0).
*/
