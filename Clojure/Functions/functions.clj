(ns user)

;GRUPA 1-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;a
(defn atomic? [v] "предикат кој vраќа true ако v не е од податочен тип колекција (collection), и false во секој друг случај.
  Се користи предикатот coll? кој враќа true за колекција, и false инаку."
  (cond
    (coll? v) false
    :else true
    )
  )

;b
(defn member? [x list] "предикат кој раќа true ако x се наоѓа во lst. Прво се врши проверка дали е празна lst, ако е враќа false.
  потоа проверува дали х се наоѓа на почеток на листата. Ако да, враќа true, ако не проверува дали првиот ел од lst е листа. Ако е,
  го повикува member? на тој прв ел(листа) и на остатокот од lst(со recur). Овие повици се во or израз за да се бара x без разлика
  дали х се наоѓа директно во lst или во нејзина подлиста. :else условот се повикува ако првиот ел на lst не е листа ниту еднаков со x.
  Овој услов проверува дали х се содржи во остатокот на lst."
  (cond
    (empty? list) false
    (= x (first list)) true
    (list? (first list)) (or (member? x (first list)) (recur x (rest list)))
    :else (recur x (rest list))
    )
  )

;v
(defn my-count [list] "предикат кој го враќа бројот на елементи во lst кои се наоѓаат на “нулто ниво”.
  Во последниот рекурзивен повик, листата е празна и овој повик враќа 0. Потоа, при враќање од рекурзијата, на резултатот од секој повик се додава 1,
  па се добива бројка која го претставува бројот на повици-кој е ист со бројот на елементи во листата(на 0то ниво)."
  (cond
    (empty? list) 0
    :else (+ 1 (my-count (rest list)))
    )
  )

;g
(defn append [lst1 lst2] "предикат кој ги спојува lst1 и lst2 во единствена листа. Ова го постигнува со вградениот предикат concat."
  (concat lst1 lst2)
  )

;d - funkcijata e prekrstena zatoa sto postoi vgradena funkcija "zip"
(defn my-zip [lst1 lst2] "предикат кој ги комбинира соодветните (на иста позиција) елементи на lst1 и lst2 во единствена листа со подлисти,
  каде секоја подлиста содржи пар елементи од двете влезни. Првите два услови проверуваат дали било која од листите е празна за да се прекине спојувањето.
  Инаку, ако не се празни, прави листа од првите ел на lst1 и lst2 и оваа листа ја става како прв елемент во листата која се добива од рекурзивниот повик врз остатокот од листите."
  (cond
    (empty? lst1) ()
    (empty? lst2) ()
    :else (cons (list (first lst1) (first lst2)) (my-zip (rest lst1) (rest lst2)))
    )
  )


;gj
(defn lookup [key list-of-pairs] "предикат кој на даден израз key (може да биде листа) и листа со подлисти list-of-pairs каде секоја подлиста има пар елементи и е во облик (key value),
  ја враќа вредноста value која соодветствува на key, или nil ако не постои таков пар. Ако листата е празна, значи нема ел соодветен на key. Се враќа nil. Инаку, се проверува дали првиот
  елемент ва листата е листа чиј прв елемент е key. Ако е, се враќа вториот ел од оваа подлиста-парот na key. Инаку, се повикува lookup на остатокот од листата за да се продолжи со брањето"
  (cond
    (empty? list-of-pairs) nil
    (= key (first (first list-of-pairs))) (second (first list-of-pairs))
    :else (recur key (rest list-of-pairs))
    )
  )

;e
(defn my-merge [lst1 lst2] "предикат кој две влезни листи од цели броеви, обете подредени по растечки редослед, ги спојува во единствена листа која исто така е подредена по растечки редослед.
  Прво проверува дали lst1 е празна. Ако е, ја враќа lst 2. Сооодветно проверува и за lst2. Ако има ел во двете листи, проверува дали првиот ел. на lst1 е помал или еднаков na првиот ел. на lst 2.
  Ako e, го става како прв елемент на досега сортираната листа што се добива од рекурзивниот повик. Ако е поголем, се извршува :else и првиот ел од lst2 се спојува со листата од повикот соодветно."
  (cond
    (empty? lst1) lst2
    (empty? lst2) lst1
    (<= (first lst1) (first lst2)) (cons (first lst1) (my-merge (rest lst1) lst2))
    :else (cons (first lst2) (my-merge lst1 (rest lst2)))
    )
  )

;zh
(defn count-all [lst] "Предикат кој го враќа вкупниот број на атомични елементи во lst, без оглед на кое ниво се наоѓаат. Ако е празна листата, се враќа 0. Инаку, ако првиот елемент на листата е
  исто така листа, треба да се врати бројот на атомични ел во него и во остатокот од листата, па се повикува count-all на двете и резултатите се враќаат како збир. Ако првиот елемент на листата
  не е листа, се додава еден на бројот на атомични ел. во остатокот од листата"
  (cond
    (empty? lst) 0
    (list? (first lst)) (+ (count-all (first lst)) (count-all (rest lst)))
    :else (+ (count-all (rest lst)) 1)
    )
  )

;z
(defn my-drop [n lst] "предикат кој ја враќа листата lst од која се отстранети првите n елементи.
  Ако е празна листата, се враќа празна листа. Ако n=0, значи поминале онолку итерации колку што треба да се отстранат елементи, па се враќа lst. Инаку,
  се повикува my-drop со еден отстранет елемент и бројач декрементиран за 1."
  (cond
    (empty? lst) ()
    (= n 0) lst
    :else (recur (dec n) (rest lst))
    )
  )

;dz
(defn my-take "Предикат кој враќа листа од првите n елементи на lst. Првата верзија на my-take враќа превртена листа од од преоптоварениот повик на my-take, со n, празна листа-акумулатор и lst.
  Втората верзија има дополнително акумулатор, во кој се зачувуваат првите n елем. од lst. Првиот услов проверува дали е празна lst, ако е се враќа акумулаторот - кој во овој случај ја содржи
  lst со обратен редослед. Ако нe 0, значи во acc се зачувале онолку елементи колку што било потребно, па се враќа acc(кој се превртува накнадно). Инаку, се повикува my-take со намален бројач-n,
  отстранет еден елемент од lst и додаден во acc."
  ([n lst] (reverse (my-take n () lst)))
  ([n acc lst]
   (cond
     (empty? lst) acc
     (= n 0) acc
     :else (recur (- n 1) (cons (first lst) acc) (rest lst))
     )
   )
  )

;i
(defn my-reverse "предикат кој ги превртува елементите од листата lst во обратен редослед. Предикатот се преоптоварува со дополнителен акумулатор pom-празна листа.
  Кога ќе се испразни lst, се враќа pom кој ја содржи lst во обратен редослед. Инаку, ако во lst има елементи, во pom на почеток се лепи првиот еллемент на lst.
  Во наредниот повик, врз првиот ел на pom(кој е ист со првиот ел од lst) се лепи вториот ел на lst-кој станува прв елемент на pom. Вака се постигнува превртувањето."
  ([lst] (my-reverse () lst))
  ([pom lst]
   (cond
     (empty? lst) pom
     :else (recur (cons (first lst) pom) (rest lst))
     )
   )
  )

;j

(defn member_lev_0? [x lst] "Функција која проверува rekurzivno дали х е елемент во list, на 0-то ниво."
  (cond
    (empty? lst) false
    (= x (first lst)) true
    :else (recur x (rest lst))
    )
  )

(defn remove-duplicates [lst] "Предикат кој ги отстранува дупликат елементите од нулто ниво на lst.
  Ако е празна lst, враќа (). Инаку, ако првиот ел од lst e член на остатокот на lst(на 0то ниво), се повикува  remove-duplicates со остатокот од lst(се губи 1от елент).
  Ако не е член, се додава првиот елемент на остатокот од листата со отстранети дупликати, која се враќа како рез. на рекурзивниот повик на остатокот од листата."
  (cond
    (empty? lst) ()
    (member_lev_0? (first lst) (rest lst)) (remove-duplicates (rest lst))
    :else (cons (first lst) (remove-duplicates (rest lst)))
    )
  )

;k
(defn my-flatten [lst] "Предикат кој отстранува едно ниво на загради од влезната листа.
  Ако е празна листата, враќа (). Инаку, ако е листа првиот ел на lst, ги спојува првиот ел на lst со остаткот од lst (која се враќа обработена на истиот начин од рекурзивниот повик).
  Инаку, ако не е листа првиот елемент, го додава како прв ел на остатокот од lst, кој се враќа соодветно обработен од рекурзивниот повик."
  (cond
    (empty? lst) ()
    (list? (first lst)) (concat (first lst) (my-flatten (rest lst)))
    :else (cons (first lst) (my-flatten (rest lst)))
    )
  )


;GRUPA 2 ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;a
(defn deliv_so [n m] "предикат кој проверува дали бројот m е далив со n со помош на вградениот предикат mod"
  (cond
    (= 0 m) false
    (= (mod m n) 0) true
    :else false
    )
  )

(defn sodrzi_cifra [n m] "предикат кој проверува дали бројот n се содржи како цифреа во бројот m.
  Најпрво сите цифри од m(кој се кастира во стринг па се дели на карактери, кои потоа се мапираат во integer) се трансформираат
  во лисѕта од integers - ch. Потоа се проверува дали n се содржи во ch."
  (let [ch (map #(Character/digit % 10) (seq (str m)))] (member? n ch))
  )

(defn buzzed [m] "Предикат враќа :buzz ако бројот m е или делив со 7 или ја содржи цифрата 7. Во спротивно го враќа бројот m."
  (cond
    (or (deliv_so 7 m) (sodrzi_cifra 7 m)) :buzz
    :else m
    )
  )

(defn buzz [list-of-ints] "предикат кој ја трансформира влезната листа од цели броеви така што секој број од листата делив со 7 и секој број кој има барем една цифра 7 го заменува
  со :buzz, додека сите останати елементи остануваат непроменети. Ова го постигнува така што секој член на листата го мапира со функцијата buzzed. "
  (map buzzed list-of-ints)
  )

;b
(defn divisors-of [n] "предикат кој за влезниот позитивен цел број n, ги враќа делителите на n, различни од 1 и самиот n. Со range генерира листа со броевите 2-n,
  па потоа ги филтрира само оние кои го делат бројот n без остаток."
  (filter #(= (mod n %) 0) (range 2 n))
  )

;c
(defn longer_str [s1 s2] "предикат кој зема два стрингови и str1 и str2 и на излез го враќа оној кој е подолг. Со if проверува дали е подолг или еднаков str1
  од str2, па ако е го враќа str1, инаку str2."
  (if (>= (count s1) (count s2)) s1 s2)
  )

(defn longest [list-of-strings] "предикат кој го враќа најдолгиот стринг од влезната листа од стрингови list-of-strings; ако постојат повеќе стрингови со максимална должина
  го враќа оној кој ќе го најде прв. Ова го постигнува со редукција на листата од стрингови со функијата longer_str."
  (reduce longer_str list-of-strings)
  )


;GRUPA 3 ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;a
(defn my-map [f lst] "предикат кој ја применува функцијата f врз секој елемент од листата lst, и враќа листа од резултати.
  Во еден повик, го прилепува повикот на f со првиот елемент од lst, на резултатот од рекурзивниот повик на my-map со остатокот од lst."
  (cond
    (empty? lst) ()
    :else (cons (f (first lst)) (my-map f (rest lst)))
    )
  )

;b
(defn my-filter [pred lst] "предикат кој го применува предикатот pred врз секој елемент од листата lst, и враќа листа од елементите кои вратиле вистина за дадениот предикат.
  Во ден повик, ако lst не е празно, го применува pred врз првиот ел., на lst. Ако овој повик врати true, го прилепува првиот ел. на lst на резултатот од рекурзивниот повик на
  my-map со остатокот од lst. Ако не е, само го повикува my-filter со остатокот на lst, па првиот ел на lst не се зачувува никаде."
  (cond
    (empty? lst) ()
    (pred (first lst)) (cons (first lst) (my-filter pred (rest lst)))
    :else (my-filter pred (rest lst))
    )
  )

;v
(defn my-reduce "предикат кој ја применува дво-аргументната функција f врз value? и првиот елемент од секвенцата lst ако воопшто се направи повик со наведен аргумент value?,
  инаку, ја применува функцијата врз првите два елементи од секвенцата.
  my-reduce е можно да се повика со 2 или 3 аргументи, па потребно е да се преоптовари. ако се повика со 2, како value се зима првиот елемент од lst, а листата која се праќа е
  остатокот од lst.
  Ако е празна lst, се враќа value, затоа што нема што да се редуцира. Инаку, предикатот f се применува врз value и првиот елемент на lst,
  a резултатот се праќа како value во понатамошната редукција на остатокот на lst."
  ([f lst] (my-reduce f (first lst) (rest lst)) )
  ([f value lst]
   (cond
     (empty? lst) value
     :else (recur f (f value (first lst)) (rest lst))
     )
   )
  )

;g
(defn my-flat-map [f lst] " my-flat-map ја применува функцијата f врз секој елемент од lst, ја израмнува резултантната листа со отстранување на едно ниво на загради и тоа го враќа како резултат.
  Ако не е празна lst, веќе обработениот остаток од lst(она што се враќа од рекурзивниот повик), го спојува со резултатот на повикот на f врз првиот елемент на lst.
  Со тоа што листите се спојуваат се постигнува да се загуби едно ниво на загради."
  (cond
    (empty? lst) ()
    :else (concat (f (first lst)) (my-flat-map f (rest lst)))
    )
  )

