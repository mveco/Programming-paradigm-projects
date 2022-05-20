%ЗАД 1 БАЗА

lice(1,petko,petkovski,m,datum(1,3,1950),kratovo,skopje).
lice(2,marija,petkovska,z,datum(30,5,1954),kumanovo,skopje).
lice(3,ljubica,petkovska,z,datum(29,11,1965),skopje,skopje).
lice(4,vasil,vasilev,m,datum(8,4,1954),bitola,bitola).
lice(5,elena,vasileva,z,datum(19,6,1958),resen,bitola).
lice(6,krste,krstev,m,datum(9,8,1948),veles,veles).
lice(7,biljana,krsteva,z,datum(13,8,1949),veles,veles).
lice(8,igor,krstev,m,datum(26,10,1971),veles,skopje).
lice(9,kristina,krsteva,z,datum(30,5,1974),kumanovo,skopje).
lice(10,julija,petrova,z,datum(30,5,1978),skopje,skopje).
lice(11,bosko,petkovski,m,datum(13,11,1981),skopje,skopje).
lice(12,gjorgji,vasilev,m,datum(15,7,1978),bitola,bitola).
lice(13,katerina,petkovska,z,datum(11,12,1979),bitola,skopje).
lice(14,petar,vasilev,m,datum(21,2,1982),skopje,skopje).
lice(15,andrej,krstev,m,datum(3,8,1998),skopje,skopje).
lice(16,martina,petkovska,z,datum(5,12,2005),skopje,skopje).
familija(1,2,[9,10]).
familija(1,3,[11]).
familija(4,5,[12,13,14]).
familija(6,7,[8]).
familija(8,9,[15]).
familija(11,13,[16]).

%а)------------------------------------------------------------------------------------
dete(LiceID, familija(T,M,L)) :- 
    lice(LiceID,_,_,_,_,_,_), familija(T,M,L), member(LiceID, L).

roditeli(LiceID, Tatko, Majka) :- dete(LiceID, familija(Tatko,Majka,_)).

grad_ragjanje(LiceID, GradR) :- lice(LiceID,_,_,_,_,GradR,_).

razlicen_grad_ragjanje_od_roditeli(LiceID):- 
    grad_ragjanje(LiceID, G), roditeli(LiceID, T,M), 
    grad_ragjanje(T, TG), grad_ragjanje(M, MG), G \= TG, G \= MG.

rodeni_razlicen_grad(Num) :- 
    findall(LiceID, razlicen_grad_ragjanje_od_roditeli(LiceID), Lica), 
    length(Lica, Num).

%б)-------------------------------------------------------------------------------------
    
ist_pol(LiceID, PredokID) :-
    lice(LiceID,_,_,Pol,_,_,_), lice(PredokID,_,_,PolP,_,_,_), 
    Pol = PolP, LiceID \= PredokID.

razlika_sedum_dena(datum(Den1,Mesec1,_), datum(Den2,Mesec2,_)) :- 
    X is Mesec1- Mesec2, 
   (X == 0, Y is Den1-Den2, Y >= -7, Y =< 7).
razlika_sedum_dena(datum(Den1,Mesec1,_), datum(Den2,Mesec2,_)) :- 
    X is Mesec1- Mesec2, 
   (X == 1, Y is 30-Den2+Den1, Y =< 7).
razlika_sedum_dena(datum(Den1,Mesec1,_), datum(Den2,Mesec2,_)) :- 
    X is Mesec1- Mesec2, 
   (X == -1, Y is 30-Den1+Den2, Y =< 7).
razlika_sedum_dena(datum(Den1,Mesec1,_), datum(Den2,Mesec2,_)) :- 
    X is Mesec1- Mesec2, 
   (X == 11, Y is 30-Den2+Den1, Y =< 7).
razlika_sedum_dena(datum(Den1,Mesec1,_), datum(Den2,Mesec2,_)) :- 
    X is Mesec1- Mesec2, 
   (X == -11, Y is 30-Den1+Den2, Y =< 7).
    

rodeni_ista_nedela(LiceID, PredokID) :- 
    lice(LiceID,_,_,_,DatumL,_,_), 
    lice(PredokID,_,_,_,DatumP,_,_), 
    razlika_sedum_dena(DatumL, DatumP),
    LiceID \= PredokID.

samo_predok(LiceID, PredokID):- roditeli(LiceID, PredokID, _).

samo_predok(LiceID, PredokID):- roditeli(LiceID, _, PredokID).

samo_predok(LiceID, PredokID) :- roditeli(LiceID, Tatko, _),
    samo_predok(Tatko, PredokID).

samo_predok(LiceID, PredokID) :- roditeli(LiceID, _, Majka),
    samo_predok(Majka, PredokID).

predok(LiceID, PredokID):-samo_predok(LiceID, PredokID),
        ist_pol(LiceID, PredokID), rodeni_ista_nedela(LiceID, PredokID).

predci(LiceID, Predci) :- findall(PredokID, predok(LiceID, PredokID), Predci).


%ЗАДАЧА 2----------------------------------------------------------------------------
telefon(111111,petko,petkovski,[povik(222222,250),povik(101010,125)]).
telefon(222222,marija,petkovska,[povik(111111,350),povik(151515,113),
                                  povik(171717,122)]).
telefon(333333,ljubica,petkovska,[povik(555555,150),povik(101010,105)]).
telefon(444444,vasil,vasilev,[povik(171717,750)]).
telefon(555555,elena,vasileva,[povik(333333,250),povik(101010,225)]).
telefon(666666,krste,krstev,[povik(888888,75),povik(111111,65),
                              povik(141414,50),povik(161616,111)]).
telefon(777777,biljana,krsteva,[povik(141414,235)]).
telefon(888888,igor,krstev,[povik(121212,160),povik(101010,225)]).
telefon(999999,kristina,krsteva,[povik(666666,110),
                                  povik(111111,112),povik(222222,55)]).
telefon(101010,julija,petrova,[]).
telefon(121212,bosko,petkovski,[povik(444444,235)]).
telefon(131313,gjorgji,vasilev,[povik(141414,125),povik(777777,165)]).
telefon(141414,katerina,petkovska,[povik(777777,315),povik(131313,112)]).
telefon(151515,petar,vasilev,[]).
telefon(161616,andrej,krstev,[povik(666666,350),povik(111111,175),
                               povik(222222,65),povik(101010,215)]).
telefon(171717,martina,petkovska,[povik(222222,150)]).
sms(111111,[222222,999999,101010]).
sms(444444,[333333,121212,161616]).
sms(111111,[777777]).
sms(666666,[888888]).
sms(444444,[555555,121212,131313,141414]).
sms(666666,[777777,888888]).
sms(888888,[999999,151515]).
sms(171717,[131313,161616]).

%a)----------------------------------------------------------------------------

povikani(X, Pov) :- telefon(X,_,_,L), 
    findall((Br, T), member(povik(Br, T), L), Pov).

povikal(X, Y, (Y,Traenje)) :- 
        povikani(X, L), member((Y,Traenje), L).

povikuvaci(X, Pov) :- 
    findall((Br, T), povikal(Br, X,(X,T)), Pov). 

soberi_vreme([(Br, T1)|L], (Br, T2), (Br, T), L) :- T is T1+T2.
soberi_vreme([X|L], (Br, T2), P, [X|L1]) :- 
    soberi_vreme(L,(Br, T2), P, L1).

spoj(L1, [], L1).
spoj(L1, [X|L2], [P|L]) :- soberi_vreme(L1, X, P, LN), spoj(LN, L2, L), !.
spoj(L1, [X|L2], [X|L]) :- spoj(L1, L2, L), !.

razgovaral_so(X, P) :- povikani(X, Pojdovni),
    povikuvaci(X, Dojdovni), spoj(Pojdovni, Dojdovni, P).

br_razgovori(L) :- 
    findall((X, Len),(razgovaral_so(X, Br),length(Br, Len)), L).

sporedi((X1, Y1), (_, Y2), (X1, Y1)) :- Y1>=Y2.
sporedi((_, _), (X2, Y2), (X2, Y2)).

najmnogu_razgovori(Т) :- br_razgovori(L), najmnogu_razgovori(Т, L).
najmnogu_razgovori(Т, [Т]).
najmnogu_razgovori(Т, [Т2|L]) :- najmnogu_razgovori(Т1, L), 
    sporedi(Т1, Т2, Т), !.

najbroj(X, Y) :- najmnogu_razgovori((Br, _)), telefon(Br,X,Y,_).

%б)----------------------------------------------------------------------------

pratil_poraka_pom(X, Poraki) :- sms(X,L), 
    findall((Br, 100), member(Br, L), Poraki).
pratil_poraka_pom(X, []).

spoj_poraki([], []). 										
spoj_poraki([X|L1], L) :- spoj_poraki(L1, LR), spoj(X, LR, L),  !.	

pratil_poraka(X, L):- 
    bagof(Poraki, pratil_poraka_pom(X, Poraki), L1), 
    spoj_poraki(L1, L).

dobil_poraka(X, L) :- 
    findall((Br, T), (pratil_poraka(Br, Poraki), member((X, T), Poraki)), L).

razmenil_poraki(X, L) :- pratil_poraka(X, Prateni), 
    dobil_poraka(X, Primeni), spoj(Prateni, Primeni, L).

ostvaril_komunikacija(X, L) :- razgovaral_so(X, Povici), 
    razmenil_poraki(X, Poraki), spoj(Povici, Poraki, L).

najmnogu_komunikacija([Т], Т).
najmnogu_komunikacija([Т2|L], Т) :- najmnogu_komunikacija(L, Т1), 
    sporedi(Т1, Т2, Т), !.

omilen(X, Y) :- ostvaril_komunikacija(X, L), najmnogu_komunikacija(L, (Y, _)).


%ЗАДАЧА 3-------------------------------------------------------------------------

klient(1,petko,petkov,[usluga(a,b,50,datum(12,12,2015),23),
                        usluga(c,a,50,datum(7,12,2015),34),
                        usluga(c,f,40,datum(7,11,2015),23)]).
klient(2,vasil,vasilev,[usluga(a,e,50,datum(25,12,2015),12),
                         usluga(c,g,40,datum(17,11,2015),56),
                         usluga(g,d,50,datum(17,12,2015),45),
                         usluga(e,a,40,datum(24,12,2015),34)]).
klient(3,krste,krstev,[usluga(c,b,60,datum(31,12,2015),56),
                        usluga(e,f,60,datum(31,12,2015),34)]).
klient(4,petar,petrov,[usluga(a,f,50,datum(25,12,2015),23),
                        usluga(f,d,50,datum(25,12,2015),34)]).
klient(5,ivan,ivanov,[usluga(d,g,50,datum(7,12,2015),56),
                       usluga(g,e,40,datum(25,12,2015),34)]).
klient(6,jovan,jovanov,[usluga(c,f,50,datum(5,12,2015),12),
                         usluga(f,d,50,datum(27,12,2015),45)]).
klient(7,ana,aneva,[usluga(e,d,50,datum(11,12,2015),12),
                     usluga(d,g,50,datum(11,12,2015),12)]).
klient(8,lidija,lideva,[usluga(e,g,50,datum(29,12,2015),45),
                         usluga(f,b,50,datum(29,12,2015),34)]).


rastojanie(a,b,4).
rastojanie(a,c,7).
rastojanie(b,c,5).
rastojanie(b,d,3).
rastojanie(c,d,4).
rastojanie(b,e,6).
rastojanie(c,e,2).
rastojanie(b,f,8).
rastojanie(e,f,5).
rastojanie(f,g,3).

%a)----------------------------------------------------------------------------

site_uslugi(L) :- findall(U, klient(_,_,_,U), L1), spoj_listi(L1, L) .

spoj_listi([], []).
spoj_listi([E|L1], L) :- spoj_listi(L1, L2), append(L2, E, L).

izbroj_lokacija(Lok,Br) :- 
    site_uslugi(L), 
    findall(Lok, member(usluga(Lok,_,_,_,_), L), L1), length(L1, Br1),
    findall(Lok, member(usluga(_,Lok,_,_,_), L), L2), length(L2, Br2),
    Br is Br1+Br2.

%б)--------------------------------------------------------------------------------

% DFS ---------------------------------------------------------------------------
pokratok((X1, Y1), (_, Y2), (X1, Y1)) :- Y1<Y2.
pokratok((_, _), (X2, Y2), (X2, Y2)).

najkratok([X], X).
najkratok([X|L], Y) :- najkratok(L, Y1), pokratok(X, Y1, Y).

dfs(K,K,T,T,0).
dfs(P,K,T,Pat,R) :- rastojanie(P,X,RX), not(member(X,T)), append(T, [X], T1),
    dfs(X,K,T1,Pat,R1), R is R1+RX.
dfs(P,K,T,Pat,R) :- rastojanie(X,P,RX), not(member(X,T)), append(T, [X], T1),
    dfs(X,K,T1,Pat,R1), R is R1+RX.

najkratok_pat(P,K,Y) :- 
    findall((Pat, R), dfs(P,K,[P],Pat,R), O), najkratok(O, Y), !.
%---------------------------------------------------------------------------------

sum([], 0).
sum([X|L], S) :- sum(L, S1), S is S1+X.

klient_kilometri(ID, Y) :- klient(ID,_,_,U), 
    findall(D, (member(usluga(P,K,_,_,_), U), najkratok_pat(P,K,(_, D))), All),
    sum(All, Y).

pogolemo((X1, Y1), (_, Y2), (X1, Y1)) :- Y1>=Y2.
pogolemo((_, _), (X2, Y2), (X2, Y2)).

najmnogu([X], X).
najmnogu([X|L], Y) :- najmnogu(L, X1), pogolemo(X, X1, Y), !.

najmnogu_kilometri(K) :- findall((ID, Y), klient_kilometri(ID, Y), L),
    najmnogu(L, (K, _)), !.

najmnogu_kilometri(X,Y) :- klient(Z,X,Y,_), najmnogu_kilometri(Z), !.

%в)------------------------------------------------------------------------------

uslugi_dekemvri2015(L) :- findall(U, klient(_,_,_,U), LU),
    spoj_listi(LU, L1), 
    findall(usluga(P,K,C,datum(Den,12,2015),D), 
            member(usluga(P,K,C,datum(Den,12,2015), D), L1), L).

uslugi_cena(L1) :- uslugi_dekemvri2015(L), 
    findall((Br, Len),
            (member(usluga(P,K,C,_,Br), L), najkratok_pat(P,K,(_, Y)), Len is Y*C),
            L1).

zarabotil(Br,P) :- uslugi_cena(L1),
    bagof(C, member((Br,C), L1), L), sum(L, P).

najmnogu_zarabotil(X) :- 
    findall((Br,P), zarabotil(Br,P), L),
    najmnogu(L, (X,_)).






    
    
    




    
    

    
