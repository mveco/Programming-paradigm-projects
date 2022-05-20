(ns user (:use clojure.test))

;--------------------ПРИМЕРИ--------------------------------------------------------
(def high-seven ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
;------------------------------------------------------------------------------------

(deftest test-suit
 (is (= "S" (suit "4S"))) ;проверка на рандом карта
 (is (= "D" (suit "QD"))) ;проверка на рандом карта
 (is (= "S" (suit "AS"))) ;проверка на рандом карта
 (is (= "H" (suit "1H"))) ;проверка на рандом карта
 (is (= "C" (suit "TC"))) ;проверка на рандом карта
)

(deftest test-rank
 (is (= 4 (rank "4S"))) ;проверка на карта со нумерички ранк
 (is (= 12 (rank "QD"))) ;проверка на карта со ранк претставен со буква
 (is (= 14 (rank "AS"))) ;проверка на карта со ранк претставен со буква
 (is (= 1 (rank "1H"))) ;проверка на карта со нумерички ранк
 (is (= 10 (rank "TC"))) ;проверка на карта со ранк претставен со буква
)

(deftest test-pair?
  (is (= true (pair? pair-hand))) ;проверка за hand која е pair
  (is (= false (pair? two-pairs-hand))) ;;проверка за hand која не е pair
)

(deftest test-three-of-a-kind?
  (is (= true (three-of-a-kind? three-of-a-kind-hand))) ;проверка за hand која е three-of-a-kind
  (is (= false (three-of-a-kind? four-of-a-kind-hand))) ;проверка за hand која не е three-of-a-kind
)

(deftest test-four-of-a-kind?
  (is (= true (four-of-a-kind? four-of-a-kind-hand))) ;проверка за hand која е four-of-a-kind
  (is (= false (four-of-a-kind? two-pairs-hand))) ;;проверка за hand која не е four-of-a-kind
)

(deftest test-flush?
  (is (= true (flush? flush-hand))) ;проверка за hand која е flush
  (is (= false (flush? straight-flush-hand))) ;;проверка за hand која не е flush
  (is (= false (flush? low-ace-straight-flush-hand))) ;проверка за специјален случај, кога Ace(вредност 14) се зема како 1."
)

(deftest test-full-house?
  (is (= true (full-house? full-house-hand))) ;проверка за hand која е full-house
  (is (= false (full-house? two-pairs-hand))) ;;проверка за hand која не е full-house
)

(deftest test-two-pairs?
  (is (= true (two-pairs? two-pairs-hand))) ;проверка за hand која е two-pairs
  (is (= false (two-pairs? full-house-hand))) ;;проверка за hand која не е two-pairs
)

(deftest test-straight?
  (is (= true (straight? straight-hand))) ;проверка за hand која е straight
  (is (= false (straight? low-ace-straight-flush-hand))) ;;проверка за hand која не е straight
  (is (= true (straight? low-ace-straight-hand))) ;проверка за специјален случај, кога Ace(вредност 14) се зема како 1."
)

(deftest test-straight-flush?
  (is (= true (straight-flush? straight-flush-hand))) ;проверка за hand која е straight-flush
  (is (= true (straight-flush? high-ace-straight-flush-hand))) ;проверка за hand која е straight-flush
  (is (= false (straight-flush? low-ace-straight-hand))) ;;проверка за hand која не е straight-flush
  (is (= false (straight-flush? flush-hand))) ;проверка за hand која не е straight-flush
  (is (= true (straight-flush? low-ace-straight-flush-hand))) ;проверка за специјален случај, кога Ace(вредност 14) се зема како 1."
)

(deftest test-value
  (is (= 1 (value pair-hand))) ;1 за pair?
  (is (= 2 (value two-pairs-hand))) ;2 за two-pairs?
  (is (= 3 (value three-of-a-kind-hand))) ;3 за three-of-a-kind?
  (is (= 4 (value straight-hand))) ;4 за straight?
  (is (= 5 (value flush-hand))) ;5 за flush?
  (is (= 6 (value full-house-hand))) ;6 за full-house?
  (is (= 7 (value four-of-a-kind-hand))) ;7 за four-of-a-kind?
  (is (= 8 (value straight-flush-hand))) ;8 за straight-flush?
  (is (= 0 (value high-seven))) ;0 за ниедна од ф-циите
)

(deftest test-kickers
  (is (= '(2 7 5 4) (kickers pair-hand))) ;за pair?
  (is (= '(4 2 7) (kickers two-pairs-hand))) ;за two-pairs?
  (is (= '(2 7 4) (kickers three-of-a-kind-hand))) ;за three-of-a-kind?
  (is (= '(6 5 4 3 2) (kickers straight-hand))) ;за straight?
  (is (= '(9 7 5 4 2) (kickers flush-hand))) ;за flush?
  (is (= '(2 5) (kickers full-house-hand))) ;за full-house?
  (is (= '(2 7) (kickers four-of-a-kind-hand))) ;за four-of-a-kind?
  (is (= '(6 5 4 3 2) (kickers straight-flush-hand))) ;за straight-flush?
  (is (= '(7 5 4 3 2) (kickers high-seven))) ;за ниедна од ф-циите
  (is (= '(5 4 3 2 1) (kickers low-ace-straight-flush-hand))) ;специјален случај, кога ACE треба да има вредност 1
  (is (= '(14 13 12 11 10) (kickers high-ace-straight-flush-hand))) ;случај кога АCE обично има вредност 14
)

(deftest test-higher-kicker?
  (is (= true (higher-kicker? '(8 7 4) '(8 5 4)))) ;true треба да врати, 7цата во 1та листа е поголема од 5та во втората листа.
  (is (= false (higher-kicker? '(8 7 4) '(8 7 4)))) ;false треба да врати, листите се идентични.
  (is (= false (higher-kicker? '(8 7 4) '(9 7 4)))) ;false треба да врати, втората листа има поголем елемент на 1та позиција
)

(deftest test-beats?
  (is (= true (beats? four-of-a-kind-hand three-of-a-kind-hand))) ;true треба да врати, појака е првата комбинација.
  (is (= nil (beats? low-ace-straight-hand high-ace-straight-hand))) ;nil треба да врати, со овој пример се проверува што се случува кога с еработи за иста комбинација
  (is (= nil (beats? flush-hand straight-flush-hand))) ;nil треба да врати, појака е втората комбинација.
)

(deftest test-winning-hand
  (is (= [] (winning-hand []))); случај со празна листа
  (is (= three-of-a-kind-hand (winning-hand three-of-a-kind-hand pair-hand))) ;случај со две комбинации
  (is (= straight-hand (winning-hand high-seven pair-hand straight-hand two-pairs-hand))) ;општ случај
  (is (= straight-hand (winning-hand low-ace-straight-hand straight-hand))) ;специјален случај, коха ACE има вредност 1
  (is (= high-ace-straight-flush-hand (winning-hand low-ace-straight-flush-hand straight-flush-hand high-ace-straight-flush-hand))) ;еден случај кога ACE зазима и вредност 1 и 14
  (is (= low-ace-straight-hand (winning-hand low-ace-straight-hand low-ace-straight-hand))) ;случај со исти комбинации
)

(run-tests)

