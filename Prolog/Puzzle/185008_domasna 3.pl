
devojka(mira).
momche(bruno).
momche(teo).
momche(igor).

levo_od(teo, nikoj).
hrana(teo, sendvich).

hrana(mira, pita).
hobi(mira, krstozbori).

maica(X, bela) :- devojka(X).

maica(bruno, zholta).

hrana(X, hamburger) :- hobi(X, pishuvanje).

sedi_pokraj(X, teo) :- hrana(X, pita).

sedi_pokraj(bruno, X) :- hrana(X, pica).

sedi_pokraj(X, Y) :- hrana(X, pica), maica(Y, bela).

hobi(igor, chitanje).

desno_od(Y, X) :- maica(X, sina), devojka(Y).

%----------------------------------------------------------------------------------
hrana(_, pica).
hobi(_, fotografija).
hobi(_, pishuvanje).
maica(_, crvena).
maica(_, sina).

sedi_pokraj(_, Y) :- momche(Y).
sedi_pokraj(_, Y) :- devojka(Y).


%2--------------------------------------------------------------------------------

ne_sodrzi((X, Y, Z, W, Lik), L) :- not(member(X, L)), not(member(Y, L)), 
    not(member(Z, L)), not(member(W, L)), not(member(Lik, L)).

licnost(X, Y, Z, W, L, K) :- 
    hrana(X, Y), hobi(X, Z), maica(X, W), 
    desno_od(X, L), L\=X, 
    ne_sodrzi((X, Y, Z, W, L), K).

licnost(X, Y, Z, W, L, K) :- 
    hrana(X, Y), hobi(X, Z), maica(X, W), 
    sedi_pokraj(X, L), L \= X,
    ne_sodrzi((X, Y, Z, W, L), K).

licnost(X, Y, Z, W, L, K) :- 
    hrana(X, Y), hobi(X, Z), maica(X, W), 
    sedi_pokraj(L, X), L \= X,
    ne_sodrzi((X, Y, Z, W, L), K).


reshenie(L) :- 
    levo_od(X1, nikoj), 
    licnost(X1, Y1, Z1, W1, X2, []), append([], [X1, Y1, Z1, W1], K),
    licnost(X2, Y2, Z2, W2, X3, K), append(K, [X2, Y2, Z2, W2], K2),
    licnost(X3, Y3, Z3, W3, X4, K2), append(K2, [X3, Y3, Z3, W3], K3),
    licnost(X4, Y4, Z4, W4, nikoj, K3), !,
    L = [(X1, Y1, Z1, W1), (X2, Y2, Z2, W2), (X3, Y3, Z3, W3), (X4, Y4, Z4, W4)].
    
