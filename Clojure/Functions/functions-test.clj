(ns user (:use clojure.test))

;GRUPA 1
(deftest test-atomic?
 (is (= true (atomic? nil))) ;nil e atomicen element
 (is (= true (atomic? 1))) ;integer e atomicen element
 (is (= true (atomic? 'a))) ;znak e atomicen element
 (is (= true (atomic? true))) ;true/false se taomicni elementi
 (is (= false (atomic? ()))) ;prazna lista e sekvenca
 (is (= false (atomic? '(a b c)))) ;lista e sekvenca, ne atomicen el
 (is (= false (atomic? #{1 2 3 4 5}))) ;set e sekvenca
)

(deftest test-member?
 (is (= false (member? 3 ()))) ; elementaren slucaj; koga listat e prazna ne moze da go sodrzi elementot
 (is (= false (member? 1 '(2 3 4)))) ;lista koja ne go sodrzi elementot
 (is (= true (member? :red '(1 4 :red :blue)))) ;lista koja go sodrzi elementot
 (is (= false (member? 'a '((b c) (d (e)))))) ;lista koja sodrzi vgnezdeni listi no ne go sodrzi elementot x
 (is (= true (member? 2 '(1 (3 (4 2)) 4)))) ;lista koja sodrzi vgnezdeni listi no i go sodrzi elementot x
 (is (= true (member? :abc '(((((((:abc)))))))))) ;lista koja sodrzi vgnezdeni listi no i go sodrzi elementot x
)

(deftest test-my-count
 (is (= 0 (my-count ()))) ;prazna lista ima 0 elementi
 (is (= 1 (my-count '((()))))) ;lista so edna vglezdena lista na 1vo nivo ima 1 element - prvata vgnezdena lista
 (is (= 2 (my-count '((b c) (d (e)))))) ;lista koja sodrzi dve vgnezdeni listi na prvo nivo ima 2 el
 (is (= 4 (my-count '(1 2 (3 4) 5)))) ;lista koja sodrzi atomicni elementi i vgnezdena lista na 1vo nivo
 (is (= 5 (my-count '(1 (() 3) a (2 (4)) ())))) ;lista koja sodrzi atomicni elementi i vgnezdena lista(prazna i polna) na 1vo nivo
)

(deftest test-append
  (is (= () (append () ()))) ;proveruva spojuvanje na prazni listi
  (is (= '(1 2 3) (append '(1) '(2 3)))) ;spojuvanje na ednostavni listi
  (is (= '(7 (3 4)) (append () '(7 (3 4))))) ;spojuvanje na lista so povekje nivoa so prazna lista
  (is (= '(1 2 3 1 (2 3) 5) (append '(1 2 3) '(1 (2 3) 5)))) ;spojuvanje na listi so povekje nivoa
  (is (= '((()) () ()) (append '((())) '(() ())))) ;spojuvanje na listi so vgnezdeni prazni listi
  )

(deftest test-my-zip
  (is (= () (my-zip() ()))) ;prazni listi, nemaat elementi za zipuvanje
  (is (= '((a 1) (b 2)) (my-zip '(a b) '(1 2)))) ;prost primer, zipuvanje na listi so ist br elementi
  (is (= '((a 1) (b 2) (c 3)) (my-zip '(a b c d) '(1 2 3)))) ;koga prvata lista ima povekje elementi
  (is (= '((a 1) (b 2)) (my-zip '(a b) '(1 2 3 4 5)))) ;koga vtorata lista ima povekje elementi
  (is (= '((a 1) (b (2 ())) ((c d (e)) 3)) (my-zip '(a b (c d (e)) e) '(1 (2 ()) 3)))) ;koga ima vgnezdeni listi, treba da se tretiraat isto kako atomicni elementi
  )

(deftest test-lookup
  (is (= nil (lookup 2 '()))) ;baranje na key-pair vo prazna lista
  (is (= nil (lookup 'a '((b 3) (c 4))))) ;baranje na pair za key kojshto ne postoi vo listata
  (is (= 2 (lookup 'b '((a 1) (b 2) (c 3))))) ;key koj postoi
  (is (= 2 (lookup () '((a 1) (2 3) (() 2))))) ;key prazna lista
  (is (= '(:red 3) (lookup '(:apple "kg") '(((:bananna "kg") (:yellow 4)) ((:pear "kg") (:green 2)) ((:apple "kg") (:red 3)))))) ;prebaruvanje vo lista kade parovite se listi
  )

(deftest test-my-merge
  (is (= () (my-merge () ()))) ;merge na prazni listi e prazna lista
  (is (= '(1 2 3) (my-merge '(1 2 3) ()))) ;merge na nekoja lista so praznae taa lista
  (is (= '(2 4 6 8) (my-merge () '(2 4 6 8)))) ;merge kade prvata lista e prazna
  (is (= '(1 2 3 5 6 8 9 10 11) (my-merge '(1 3 6 8 9) '(2 5 10 11)))) ;merge na dve listi
  (is (= '(-3 0 1 1 2 5 7 7 11 12) (my-merge '(-3 0 1 5 7 7 11) '(1 2 12)))) ;merge na dve listi kade se povtoruvaat elementi
  )

(deftest test-count-all
  (is (= 0 (count-all () ))) ;vo prazna lista nema voopsto atomicni elementi
  (is (= 3 (count-all '(1 2 3) ))) ;vo lista so edno nivo, sostavena od atmicni elemeti, se broj sekoj el
  (is (= 0 (count-all '(()) ))) ;vo vgnezdeni prazni listi, bez atomicni elementi
  (is (= 5 (count-all '(1 (2 3) ((4 (5)))) ))) ;slozena struktura od vgnezdeni listi koi sodrzat slozeni elementi
  (is (= 2 (count-all '((true) ((() (:abc)))) ))) ;slozena struktura na prazni listi i listi koi sodrzat atomicni el
  )

(deftest test-my-drop
  (is (= () (my-drop 3 () ))) ;otstrani el od prazna lista
  (is (= '(1 2 3) (my-drop 0 '(1 2 3) ))) ;otstranuvanje na 0 elementi od lista-samata lista e rezultat
  (is (= () (my-drop 6 '(a b c 3 4) ))) ;otstranuvanje na povekje el od samata dolzina na listata - ostanuva prazna lista
  (is (= '(:maroon :orange) (my-drop 3 '(:red :blue :yellow :maroon :orange) ))) ;regularen primer
  (is (= '(((() (:abc)))) (my-drop 1 '(((true)) ((() (:abc)))) ))) ;otstranuvanje na el od lista so povekje nivoa
  )

(deftest test-my-take
  (is (= () (my-take 3 () ))) ;od prazna lista ne moze da se zemat elementi
  (is (= '(1 2 3) (my-take 5 '(1 2 3) ))) ;zimanje na povekje elementi otkolku sto sodrzi listata
  (is (= () (my-take 0 '(a b c 3 4) ))) ;0 elementi od nekoja lista e prazna lista
  (is (= '(:red :blue :yellow) (my-take 3 '(:red :blue :yellow :maroon :orange) ))) ;regularen primer na zimanje n elementi od lista koja sodrzi povekje od ne elementi
  (is (= '(((true))) (my-take 1 '(((true)) ((() (:abc)))) ))) ;zimanje na el od lista so povekje nivoa
  )

(deftest test-my-reverse
  (is (= () (my-reverse () ))) ;reverse od prazna lista e prazna lista
  (is (= '(3 2 1) (my-reverse '(1 2 3) ))) ;reverse na lista koja ne sodrzi elementi-lista
  (is (= '(4 3 c b a) (my-reverse '(a b c 3 4) ))) ;reverse na lista koja ne sodrzi elementi listi
  (is (= '(((() (:abc))) ((true))) (my-reverse '(((true)) ((() (:abc)))) ))) ;reverse na lista koja e sostavena od listi
  (is (= '(4 (a (b)) 2 (c)) (my-reverse '((c) 2 (a (b)) 4)))); reverse na lista koja sodrzi atomicni i listi elementi
  )

(deftest test-remove-duplicates
  (is (= () (remove-duplicates () ))) ;vo praznata lista nema duplikati
  (is (= '(1 2 (4 5) 3) (remove-duplicates '(1 2 (4 5) 3) ))) ;lista koja nema duplikati se vrakja nepromeneta
  (is (= '(1 :red 2 a) (remove-duplicates '(1 2 :red 2 a a) ))) ;otstranuvanje na duplikati od lista so edno nivo
  (is (= '(2 1 (a (b))) (remove-duplicates '(1 2 (a (b)) 1 (a (b))) ))) ;i listite koi se slementi mozat da bidat duplikati
  (is (= '(1 2 (2 (1)) 3) (remove-duplicates '(1 2 (2 (1)) 3) ))) ;se otstranuvaat duplikatite samo na nulto nivo
  )

(deftest test-my-flatten
  (is (= () (my-flatten () ))) ;specijalen slucaj, prazna lista
  (is (= '(1 2 4 5 3) (my-flatten '(1 2 (4 5) 3) ))) ;lista koja sodrzi lista i atomicni elementi
  (is (= '(1 2 :red 2 a a) (my-flatten '(1 2 :red 2 a a) ))) ;listata koja ne sodrzi listi ostanuva nepromeneta
  (is (= '() (my-flatten '(() () ()) ))) ;listata sostavena od prazni listi na 1vo nivo se izramnuva vo prazna lista
  (is (= '(a ()) (my-flatten '((a) (()) ()) ))) ;listat koja sodrzi prazna lista vgnezdena vo n-nivoa povtorno ke ja sodrzi taa lista no na n-1 nivo
  (is (= '((1 1) (2 3) (5 7)) (my-flatten '(((1 1) (2 3)) ((5 7))) ))); lista od listi
  )

;GRUPA 2

(deftest test-buzz
  (is (= () (buzz ()))) ;prazna lista
  (is (= '(0 0) (my-flatten '(0 0) ))); lista 0d nuli, specijalen slucaj (ima mod 0 pri delenje so 7, no 0 ne e deliv so 7)
  (is (= '(1 2 4 5 3) (buzz '(1 2 4 5 3) ))) ;lista koja ne sodrzi broj deliv so 7 ili broj koj ima cifra 7, ostanuva nepromeneta
  (is (= '(4 :buzz 3 :buzz 55 :buzz) (buzz '(4 7 3 14 55 49) ))) ;listata koja sodrzi broevi delivi so 7, treba da se zamenat so :buzz
  (is (= '(55 :buzz :buzz 40 :buzz) (buzz '(55 17 71 40 777) ))) ;lista koja sodrzi bri=oevi koi sodrzat cifra 7, treba da se otstranat
  (is (= '(:buzz 30 :buzz :buzz 8 0 :buzz) (buzz '(17 30 28 35 8 0 672) ))) ;lista koja sodrzi broevi delivi so 7 i broevi koi imaat cifra 7, tie se zamenuvaat so :buzz
  )

(deftest test-divisors-of
  (is (= () (divisors-of 0))) ;0 e specijalen slucaj, ne treba da se vrati nisto
  (is (= () (divisors-of 1))) ;1 e specijalen slucaj, ne treba da go vrati 1 deka mu e delitel(vrakja deliteli osven 1 i samiot sebe
  (is (= () (divisors-of 13))) ;broj koj nema deliteli osven 1 i samiot sebe ne treba da vratis nishto
  (is (= '(2 4 7 14) (divisors-of 28))) ;regularen broj koj ima deliteli osven samiot sebe i 1
  (is (= '(3 11) (divisors-of 33))) ;regularen broj koj ima deliteli osven samiot sebe i 1
  )

(deftest test-longest
  (is (= "a" (longest '("a")))) ;vo lista od eden string, toj e najdolg
  (is (= "12" (longest '("" "12" "1")))) ;lista koja sodrzi stringovi, site so razlicna dolzina
  (is (= "345" (longest '("345" "12" "345")))) ;lista koja sodrzi duplikati
  (is (= "cde" (longest '("ab" "cde" "4" "l a" "1")))) ;koga ima dve ili povekje najdolgi listi, se zima prvata najdolga lista
  (is (= "      " (longest '("abcde" "      " "123")))) ;string od prazno mesto
  )

;GRUPA 3

(deftest test-my-map
  (is (= () (my-map odd? ()))) ;osnoven slucaj, prazna lista
  (is (= '(true false true false true) (my-map odd? '(1 2 3 4 5)))) ;proverka so predefinirana fja kako argument
  (is (= '(1 4 9 16 25) (my-map #(* % %) '(1 2 3 4 5)))) ;proverka so korisnicki definirana fja kako argument
  (is (= '(true true false true false) (my-map coll? '((1 2 3) #{a b c} 11 [:red :blue] "abc")))) ;proverka so lista koja sodrzi atomicni i neatomicni elementi
  (is (= '(6 7 8 9 10) (my-map inc (range 5 10) ))) ;proverka so lista koja se generira
  )

(deftest test-my-filter
  (is (= () (my-filter even? ()))) ;osnoven slucaj, prazna lista
  (is (= '(2 4 6) (my-filter even? '(1 2 3 4 5 6)))) ;proverka so predefinirana fja kako argument
  (is (= '(11 80 29) (my-filter #(> % 10) '(5 11 80 3 29 1)))) ;;proverka so korisnicki definirana fja kako argument
  (is (= '(11 "abc" :yellow) (my-filter atomic? '((1 2 3) #{a b c} 11 [:red :blue] "abc" :yellow)))) ;proverka so lista koja sodrzi atomicni i neatomicni elementi
  (is (= '(6 8) (my-filter #(= 0 (mod % 2)) (range 5 10) ))) ;proverka so lista koja se generira
  )

(deftest test-my-reduce
  (is (= nil (my-reduce + ()))) ;osnoven slucaj, prazna lista kako arg
  (is (= 5 (my-reduce + '(5)))) ;osnoven slucaj, lista so eden element
  (is (= 4 (my-reduce + 4 ()))) ;osnoven slucaj, prazna lista kako arg, plus opcionalen argument
  (is (= 8 (my-reduce + 3 '(5)))) ;osnoven slucaj, lista so eden element, plus opcionalen argument
  (is (= '(1 2 3 a (b c) 11 12) (my-reduce concat '((1 2 3) () (a (b c)) (11 12))))) ;rabota so listi
  (is (= 500 (my-reduce * '(5 10 2 5)))) ;proverka bez opcionalen paramatar i integers
  (is (= 1000 (my-reduce * 2 '(5 10 2 5)))) ;proverka so opcionalen paramatar i integers
  (is (= 42 (my-reduce #(+ (* %1 2) (- %2 1)) '(1 2 3 4 5)))) ;proverka so korisnicki def fja
  )


(deftest test-my-flat-map
  (is (= () (my-flat-map seq ()))) ;osnoven slucaj, prazna lista
  (is (= '(\a \b \c \1 \2 \e \l \e \v \e \n) (my-flat-map seq '("abc" "12" "eleven")))) ;proverka so predefinirana fja i string
  (is (= '(0 0 1 0 1 2 0 1 2 3 0 1 2 3 4) (my-flat-map range '(1 2 3 4 5)))) ;proverka so predefinirana fja i integer
  (is (= '(1 2 3 a b c m (n)) (my-flat-map my-flatten '(((1 2) (3)) ((a b c) (m (n)))) ))) ;proverka so korisnicki def fja i lista od listi
  (is (= '(\k \r \a \j \space \k \o \n \e \c \n \o) (my-flat-map seq '("kraj " "konecno") ))) ;proverka
  )

(run-tests)
