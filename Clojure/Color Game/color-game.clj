(ns user)

(defn rotate-row [row] "Редот го ротира за 1 место налево, првиот ел го става како последeн."
  (concat (rest row) [(first row)])
  )

(defn decrement-counter [counter row-no initial] "Counter e листа, каде секој елемет е бројач за соодветниот ред. row-no е бр. на ред за кој треба да се декрементира бројачот.
  за сите подолни редици треба да се рестартира  бројачот на вредноста initial."
  (cond
    (empty? counter) ()
    (= 1 row-no) (cons (dec (first counter)) (decrement-counter (rest counter) -1 initial))
    (= -1 row-no) (cons initial (decrement-counter (rest counter) -1 initial))
    :else (cons (first counter) (decrement-counter (rest counter) (dec row-no) initial))
    )
  )


(defn check-all-value-in-column-different? [column] "Column претставува листа. Проверува дали сите вредности во неа се различни."
  (not-any? #(not= 1 %) (vals (frequencies column)))
  )

(defn check-different-columns? [rows] "Проверува дали во сите колони се различни елементи"
  (cond
    (empty? (first rows)) true
    :else
      (let [compare-column (map first rows) columns (map rest rows)]
        (cond
          (check-all-value-in-column-different? compare-column) (check-different-columns? columns)
          :else false
          )
        )
    )
  )

(defn solve "на влез добива сложувалка, на излез соодветно решената сложувалка или NO SOLUTION ако нема решение на сложувалката.
  Заради оптимизација, првиот ред на сложувалката не се ротира, сите можни решенија можат да се прилагодат на тој ред ако е непроменлив.
  повикувањето на solve со [color] ваднаш ја повикува color со 4 аргументи: [head position tail counter] :
  position е моменталната позиција на редот кој треба да се прилагоди за да нема конфликт со погорните, head се погормните редови во кои нема конфликт,
  tail е редот кој се обраборува на прво место плус останатите редови, counter e counter е листата која води сметка за ротациите во секој ред.
  На почеток counter е листа со бројки кои го претставуваат бројот на ел во секој ред.
  При ротација на еден ред, неговата вредност во counter се намалува за 1. Кога вредноста на тој ред ќе дојде до 0(или помалку), значи дека нема решение, па треба да се поместат погорните редови.
  Во тој случај треба да се ротира погорниот ред додека повторно не се најде решение, а бројачите на подолните редови да се рестартираат.
  Задачата нема решение кога на ред за ротирање ќе дојде првиот ред. "
  ([color] (solve [(first color)] 2 (rest color) (take (count color) (repeat (count (first color)))) )) ;како поминат ред го зима првиот, 2 е позиција на 2от ред-кој се проверува,
  ;остаток на матрицата заедно со моменталниот ред е (rest color), а (take (count color) (repeat (count (first color)))) ова генерира листа со онолку елементи колу што има редици.
  ;Секој елемент има вредност (count (first color)) - онолку колку што има елементи првиот ред(и сите останати редови).
  ([head position tail counter]
   (cond
     (= 1 position) "NO SOLUTION" ;се проверува дали следен за вртање е 1от ред, ако е треба да се запре, нема решение.
     (empty? tail) head ;ако нема останати редови, врати ја главата-решението на сложувалката
     (check-different-columns? (concat head [(first tail)])) (recur (concat head [(first tail)]) (inc position) (rest tail) counter) ;ако нема конфликт во моменталната редица и погорните редици,
     ;повикај ја solve со head сите редици пред моменталнава и моменталнава залепена на крај, tail сите редици по моменталнава, позцијата зголемена за 1 и бројачот непроменет.
     :else ; се извршува ако има конфликт меѓу моменталната редица и погорните.
       (let [row (rotate-row (first tail)) new-counter (decrement-counter counter position (count row))] ;row е моменталниот ред соодветво ротиран, new-counter е бројачот соодветно промент
         (cond
           (>= 0 (nth new-counter (dec position))) (recur (drop-last head) (dec position) (cons (rotate-row (last head)) tail) (decrement-counter new-counter (dec position) (count row)))
           ;(dec position) се користи затоа што nth почнува од 0. Ако е извртен целиот ред, неговиот бројач ќе биде 0(или помал), треба да се вратиме еден ред нагоре.
           ;се повикува solve со редот над моменталниот, соодветно ротиран и со соодветно променет бројач, со соодветни head и tail.
           (check-different-columns? (concat head [row])) (recur (concat head [row]) (inc position) (rest tail) new-counter) ; ако row сега одговара на погорните редови,
           ;се променува моменталниот ред со row и solve продолжува понатаму.
           :else (recur head position (cons row (rest tail)) new-counter) ;инаку, повторно се повикува solve за моменталниот ред, за да продолжи со вртење додека не најде решение
         )
     )
   )
  )
 )






