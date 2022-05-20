%Задача 1
%Основен случај
odd(X) :- X==1.
%Во секој рекурзивен повик како аргумент се задава Y = Х-2, за евентуално да се дојде до елементарниот случај, ако Х е непарен број, или пак да се добие неточно на проверката  Y>0(кога Y ќе има вредност 0) ако Х е парен број.
odd(X) :- Y is X-2, Y>0, odd(Y), !. 

neparen_palindrom(L) :- reverse(L2, L),  neparen_palindrom(L, L2, 0).
%Основен случај, кога двете листи успешно се испразниле и бројачот е непарен број(значи во листите имало непарен бр на елементи). 
neparen_palindrom([], [], X):- odd(X), write(yes).
%Проверка дали првите елементи од две листи се исти, зголемување на бројачот за еден и рекурзивен повик на  neparen_palindrom со остатокот од листите и зголемениот бројач.
neparen_palindrom([H1|O1],[H2|O2], X) :- H1==H2, Y is X+1,
    neparen_palindrom(O1, O2, Y).

%Задача 2

prefix(_, []). 
prefix([H|L], [H|L1]):- prefix(L, L1), !. 

zemi_podniza(_, 0, []).
zemi_podniza([H1|L], X, [H1|L1]):- Y is X-1, zemi_podniza(L, Y, L1).

broj_pojavuvanja_podniza([], _, 0).
broj_pojavuvanja_podniza([H|L],[H|L1], X) :- prefix(L, L1),
    broj_pojavuvanja_podniza(L, [H|L1], Y), X is Y+1, !.
broj_pojavuvanja_podniza([H|L],[H1|L1], X):-broj_pojavuvanja_podniza(L, [H1|L1], X).

nаj_podniza(L1,N,L2) :-  naj_podniza(L1,N,L2,P).
 
naj_podniza([H|L1], N, L2,P2) :- naj_podniza(L1,N,L2,P2), 
    zemi_podniza([H|L1], N, Lnova), 
    broj_pojavuvanja_podniza([H|L1], Lnova, P), P2>=P, !.
naj_podniza([H|L1], N, L2,P2) :-
    zemi_podniza([H|L1], N, L2), 
    broj_pojavuvanja_podniza([H|L1], L2, P2).

%Задача 3
%Ги проверува елементарните случаеви за да врати не ако листата има помалку од 2 елементи
proveri([]):-write(no).
proveri([_]):-write(no).

%Го повикува предикатот proveriPogolem(H, L), за да провери дали првиот е поголем од вториот елемент во L.
proveri([H|L]) :- proveriPogolem(H, L).

proveriPogolem(_, []):- write(yes),!.
proveriPogolem(H1, [H2|L]) :- H1 < H2, proveriPomal(H2, L), !.
proveriPogolem(H1, [H2|L]):- write(no), !.

proveriPomal(_, []):- write(yes), !.
proveriPomal(H1, [H2|L]) :- H1 > H2, proveriPogolem(H2, L), !.
proveriPomal(H1, [H2|L]) :- write(no), !.

%Задача 4
dodadi_element_na_pozicija(X, L, 0, [X|L]).
dodadi_element_na_pozicija(X, [H|L], P, [H|L1]) :- P1 is P-1, 
    dodadi_element_na_pozicija(X, L, P1, L1), !.

dodadi_element_na_sekoja_pozicija(X, L, L1) :- length(L, P), 
    dodadi_element_na_sekoja_pozicija(X, L, P, L1).

%Во секој повик, на L1 се додава L трансформирано( променливата Е) со предикатот погоре(за една позиција), и се додава на L1. Потоа се повикува истото за помала позиција.
dodadi_element_na_sekoja_pozicija(X, L, 0, [L1]) :- 
    dodadi_element_na_pozicija(X, L, 0, L1).
dodadi_element_na_sekoja_pozicija(X, L, P, [E|L1]) :- 
    dodadi_element_na_pozicija(X, L, P, E),
    P2 is P-1, dodadi_element_na_sekoja_pozicija(X, L, P2, L1), !.

dodadi_element_na_sekoja_pozicija_lista(_, [], []).
dodadi_element_na_sekoja_pozicija_lista(X, [H|L], L1) :- 
    dodadi_element_na_sekoja_pozicija(X, H, H1), 
    dodadi_element_na_sekoja_pozicija_lista(X, L, L2),
    append(H1, L2, L1).

permutacii([E], [[E]]).
permutacii([H|L], L2) :- permutacii(L, L1), 
    dodadi_element_na_sekoja_pozicija_lista(H, L1, L2).

%Задача 5
dodaj_nuli(L1, 0, L1).
dodaj_nuli(L1, N, [0|L2]) :- N1 is N-1, dodaj_nuli(L1, N1, L2), !.

%Во X is Len1-Len2 се зачувува разликата од должините на L1 и L2. Ако  X>=0, значи L1 е подолга од L2 и треба да и се додадат Х нули на почеток на L2 за да листите да имаат иста должина.
izramni_listi(L1, L2, L1, K2) :- length(L1, Len1), length(L2, Len2),
    X is Len1-Len2, X>=0, dodaj_nuli(L2, X, K2), !.

%Ако  X<0(се подразбира), значи L1 е пократка од L2 и треба да и се додадат Х нули на почеток на L1 за да листите да имаат иста должина.
izramni_listi(L1, L2, K1, L2) :- length(L1, Len1), length(L2, Len2),
    X is Len2-Len1, dodaj_nuli(L1, X, K1), !.

%Собирање----------------------------------------------------------------------
%Основен случај
sobiranje([],[],0,[]).
sobiranje([X|L1], [X|L2], Y, [Y|L]) :- 
    sobiranje(L1, L2, Y, L), Y is X, !.
sobiranje([X1|L1], [X2|L2], X, [1|L]) :- sobiranje(L1, L2, Y, L), 
    X is X1+X2+Y-1, X is 0, !.
sobiranje([X1|L1], [X2|L2], X, [0|L]) :- sobiranje(L1, L2, Y, L), 
    X is X1+X2+Y-1, X is 1, !.
%Ако пренесената цифра е 0, собирањето е завршено, збирот е во L.
sobiranje(L1, L2, L) :- izramni_listi(L1,L2,K1,K2), 
    sobiranje(K1, K2, 0, L), !.
%Ако пренесената цифра е 1, треба да се додаде една единица на почеток на L-збирот.
sobiranje(L1, L2, [1|L]) :- izramni_listi(L1,L2,K1,K2), 
    sobiranje(K1, K2, 1, L).
%Одземање--------------------------------------------------------------------
%Основен случај
odzemanje([],[],0,[]).
odzemanje([H1|L1], [H2|L2], X, [0|L]) :- odzemanje(L1,L2,Y,L), 
    X is H1-H2-Y, X is 0, !.
odzemanje([H1|L1], [H2|L2], X, [1|L]) :- odzemanje(L1,L2,Y,L), 
    X is H1-H2-Y + 2, X is 1, !.
odzemanje([1|L1], [0|L2], Y, [1|L]) :- odzemanje(L1,L2,Y,L), Y is 0, !.
odzemanje([0|L1], [1|L2], Y, [0|L]) :- odzemanje(L1,L2,Y,L), Y is 1, !.

%ако по завршување на одземањето пренесената цифра е 0, значи L1 е поголем или еднаков на L2 и резултатот е позитивен број.
odzemanje(L1,L2,L) :- izramni_listi(L1,L2,K1,K2), 
    odzemanje(K1, K2, 0, L), !.

%ако по завршување на одземањето пренесената цифра е 1, значи L1 е помал од  L2 и резултатот е негативен број, зчачи треба да се врати [0] во L.
odzemanje(L1,L2,[0]) :- izramni_listi(L1,L2,K1,K2), 
    odzemanje(K1, K2, 1, L).

%Множење--------------------------------------------------------------------------
%Ако Х=1, append(L1, [0], L3) – дејствува како множење на L1 со 10 и резултат во L3.
%sobiranje(L3,L2,L) – го собира L3 со L2.
pomnozi_i_soberi(L1, 1, L2, L) :- append(L1, [0], L3), 
    sobiranje(L3,L2,L).

%Ако Х=0,  L1*X*10 + L2 = L2, па враќа L2.
pomnozi_i_soberi(L1, 0, L2, L2).

%Основни случаи
mnozenje(L1, [0],[0]).
mnozenje(L1, [1], L1).

%За секоја цифра Н од L2, во К се враќа производ од L1 и L2 во минатиот повик.
%Потоа К се множи со 10 и се собира со L1, за да се добие производот од L1 и L2(L) во овој повик.
mnozenje(L1, [H|L2], L) :- mnozenje(L1, L2, K), 
    pomnozi_i_soberi(L1, H, K, L).

%МИСЛАМ ДЕКА ПРИМЕРОТ ?-mnozenje([1,1,0],[1,1],L) е грешен

%Делење ---------------------------------------------------------------------------

%Пред да се споредуваат листите треба да се израмнат за да имаат ист број на позиции.
pogolem(L1, L2) :- izramni_listi(L1, L2, K1, K2), е_pogolem(K1, K2).

%основен случај, ако секоја цифра од листите е иста, тогаш листите се исти.
е_pogolem([], []).

%Ако во L1 се појави 1 а во L2- 0, значи L1 е поголемо, врати точно.
е_pogolem([1|L1], [0|L2]).

%Ако на иста позиција во листите има ист елемент - Х, продолжи со споредбата понатаму.
е_pogolem([X|L1], [X|L2]):- е_pogolem(L1, L2), !.

%----------------------------------------------------------------------------

% pogolem(L1, L2) враќа true, значи  L1 е поголем од L2 и може да се дели.
% odzemanje(L1, L2, Lrez) – од L1 го одзема L2 и резултатот го сместува во Lrez.
% delenje(LRez, L2, R1) – Повикува делење на Lrez со L2 и резултатот го враќа во R1. R1 претставува колку пати L2 се содржи во Lrez.
%sobiranje(R1, [1], R) – на R1 му додава 1 за да го добие бројот на појавувања-R на L2 во L1.
delenje(L1, L2, R) :- pogolem(L1, L2), 
    odzemanje(L1, L2, LRez), 
    delenje(LRez, L2, R1), 
    sobiranje(R1, [1], R), !.

%Основен случај, pogolem(L1, L2) вратило False, значи делењето е завршено.
delenje(_, _, [0]).

%Задача 6

pomnoziRedSoKolona([], [], 0).
pomnoziRedSoKolona([R|Red], [K|Kolona], N) :-
    pomnoziRedSoKolona(Red, Kolona, M), N is M+R*K.

generirajRed(_,[],[]).
generirajRed(Red, [M|Matrica], [N|NovRed]) :- 
    pomnoziRedSoKolona(Red, M, N), 
    generirajRed(Red, Matrica, NovRed).

presmetaj(M, R) :- presmetaj(M, M, R).

%Елементарен случај, ако матрицата е 1х1.
presmetaj([E], [R]) :- R is E*E, !.

presmetaj([], _, []).
presmetaj([Red|M], MT, [NovRed|R]) :- 
    generirajRed(Red, MT, NovRed), 
    presmetaj(M, MT, R), !.

%задача 7

%Прво споредува која е подолга листа 
compare_lists(L1, L2, 1) :- length(L1, X1), length(L2, X2), X1>X2, !.
compare_lists(L1, L2, 2) :- length(L1, X1), length(L2, X2), X1<X2, !.

%Ако листите се со иста должина ги споредува соодветните елемнти во листите додека не нејде во која листа има поголем елемент. Ако листите с еидентични враќа 0.
compare_lists([], [], 0).
compare_lists([H1|L1], [H2|L2], 1) :- H1>H2, !.
compare_lists([H1|L1], [H2|L2], 2) :- H1<H2, !.
compare_lists([H1|L1], [H2|L2], T) :- compare_lists(L1, L2, T).

%Во случај кога споредбата враќа 1, значи Е треба да се пропагира понатаму, не му е местото пред Н.
add_element([H|L1], E, [H|L2]) :- compare_lists(H,E,X), X is 1, 
    add_element(L1, E, L2), !.

%Во случај кога споредбата враќа 0, значи L ја содржи Е и ништо не треба да се вметне
add_element([H|L1], E, [H|L1]) :- compare_lists(H,E,X), X is 0.

%Во случај кога споредбата враќа 2(се подразбира), значи Е е поголем од првиот елемент на L и Е треба да се вметне на почеток.
add_element(L, E, [E|L]).

transform([E], [E]). 
transform([H|L], T) :- transform(L, T1), add_element(T1, H, T), !.

%Задача 8
%Кога ќе го најде елементот од E во L1, како резултат ја враќа L1 без Е.
otstrani_element(E,[E|L1],L1).
%Го бара E.
otstrani_element(E,[E1|L1],[E1|L2]) :- otstrani_element(E,L1,L2), !.

e_lista([]).
e_lista([_|_]).

brisi_sekoe_vtoro(L, R) :- brisi_sekoe_vtoro(L,R, [], NEL).

%Основен случај, кога ќе се испразни L.
brisi_sekoe_vtoro([], [], EL, EL).

%Кога елементот Е во листата L е листа, треба да се повика предикатот и за тој елемент.
%Важно е кога обработката на Е ќе заврши, во резултантната листа R да се додаде Е1-резултатот од обработката на E, а листата на елементи кои се појавуваат непарен број пати-NEL да се проследи во понатамошната обработка на L.
brisi_sekoe_vtoro([E|L], [E1|R], EL, FEL) :- e_lista(E),
    brisi_sekoe_vtoro(E, E1, EL, NEL), 
    brisi_sekoe_vtoro(L, R, NEL, FEL), !.

%Ако otstrani_element(E, EL, NEL) врати точно, значи Е се наоѓал во ЕL, се отстранил и резултатот е во NEL. NEL е актуелната листа на елементи кои треба да се отстранат и таа се прследува понатаму. Соодветно,во R не се додава Е.
brisi_sekoe_vtoro([E|L], R, EL, FEL) :- 
    otstrani_element(E, EL, NEL),
    brisi_sekoe_vtoro(L, R, NEL, FEL), !.

%Ако otstrani_element(E, EL, NEL) врати неточно(се подразбира), значи E не се наоѓал во EL - тој се појавува непарен пат и не треба да се отстрани.
%[E|EL] – се додава во  EL и се додава во R: [E|R].
brisi_sekoe_vtoro([E|L], [E|R], EL, FEL):-
    brisi_sekoe_vtoro(L, R, [E|EL], FEL), !. 







