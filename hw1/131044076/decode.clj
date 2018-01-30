; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Yakup Genc                       *
; *********************************************

;; utility functions
 ;; "c2i and "i2c"


(use 'clojure.java.io)
;(import 'java.util.concurrent.Executors)

(defn read-as-list "dosyadan okuduklarını liste içerisine aktarır" [filename]
 (let [ liste ( clojure.string/split
                (clojure.string/lower-case
                  (slurp filename) ) #"\W+" )  ]
   (for [item liste ] (map symbol (map str (apply str item))))))

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***
(defn spell-checker-1 "this function will search word in dictionary2.txt with use .indexOf function"   [word]
  (not= (.indexOf   (read-as-list "dictionary2.txt")   word) -1))


(defn spell-checker-0 "this function will search word in dictionary2.txt with using lineer searching algorithm"    [word] (let [kelimeler (read-as-list "dictionary2.txt")]
                               (contains?
                                 (set (for [x kelimeler :when (= word x)]
                                        (boolean true))) true) ))

(defn permutations "this create all permutations of s ,this taken from a website"   [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))
(defn Replace-Verify-Paragraph "This convert encoded words to decoded word then verify them in spell-checker-1" [paragraph permutation_list]
      (for [x paragraph]
        (let [newword (Replace-Word x permutation_list  )]
          (cond (= (spell-checker-1 newword) true )(boolean  true)
                :else (boolean false)))))


(defn Find-in-Permutation-list "this find the map which encoded letter is [harf] in permutasyon_list" [harf permutasyon_list](let [arama-sonuc (.indexOf  (map first permutasyon_list) (str harf))]
                                                           (cond (= arama-sonuc -1 ) false
                                                                 :else (second (nth permutasyon_list arama-sonuc)))))


(defn Replace-Word "this convert encoed word to decoded word according to permutasyon_list if the letter is not found in permutation_list
  it will be same" [word permutation_list](let [sonuc (fn [kelime]
                                                           (for [x kelime ]
                                                             (let [harfs (Find-in-Permutation-list x permutation_list)]
                                                               (cond (= harfs false )   x
                                                                     :else  (symbol (apply str harfs ))))))]
                                              (sonuc word)))
(defn binarySearch  "This find word with binarySearch algorithm" [word liste]
    (if (= (count  liste)  0 )false
      (let [ nword (map symbol (map str word))](loop [ilk 0 son (count liste)] (let [ center (/ (+ ilk son) 2) centeritem (nth liste center )]
                                                                            (cond (= centeritem nword) true
                                                                                  (and (= (count liste) 1)  (not= centeritem nword)) false
                                                                            (< (compare ( str centeritem) ( str nword)) 0) (recur center son)
                                                                             :else (recur ilk center ))
                                                 )))))

(defn Find-true-permutation_list "Function will convert letters with permutation lists and check all paragraph then return list of rate of verify"
  [paragraph permutasyon-groups]
   (sort-by first >  ( remove nil?
                       (set
                         (for  [perm permutasyon-groups ]
                           (let [sonuc (Replace-Verify-Paragraph paragraph perm)
                                oran (/ (count (filter true? sonuc)) (count paragraph)) ]
                                (cond (>= oran  0.5 ) (list oran,perm)  )))))) )

(defn RemoveFromAlfabe "ths take two set which remove intersecting letters" [alfabe removedlist]
  (let [a alfabe  b removedlist  ]
    (remove (set b) (set a))))


(defn findAllPermutation "this used in Gen-Decoder-B-0 to find all permutations which count is 20!x6!"
	[paragraph]

(lazy-seq (let [engFrekans (permutations '("e" "t" "a" "o" "i" "n")) alfabe '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"  "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
          paragraphFrekans   (map first (sort-by second > (frequencies (map str
                                                                                (for [letter paragraph x1 letter]
                                                                                  (str x1))))))
       paragraphFrekanstop  (take 6 (map first (sort-by second > (frequencies (map str
                                                                                (for [letter paragraph x1 letter]
                                                                                                             (str x1)))))))
       frekans-perm-grup (  for [x engFrekans ]
                                    (map list paragraphFrekanstop x ))
       bruth-force-perm (for [x (permutations (RemoveFromAlfabe alfabe '("e" "t" "a" "o" "i" "n")))]
                           (map list (RemoveFromAlfabe alfabe paragraphFrekans) x  ))
       allpermutations (map concat bruth-force-perm frekans-perm-grup )]bruth-force-perm)))
;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defn Gen-Decoder-A "Öncelikle encoded ve decode için kullanılmış harfler listesi oluşturulur. Ardından bu harflar birbirleri arasında map edilip
  bir liste olarak tüm permutasyonların listesini oluşturur.  Oluşturduğu tüm permutasyonları birer birer encoded edilmiş dosya üzerinde dener
  ve spell-checker-1 den gelen true sayısınagöre %50 uyuşma gösteren tüm permusyonları, oranları ile birlikte bir liste halinde tutar
  en son bunlar içinde oranı en yüksek olanı doğru permutasyon listesi olarak seçer. Geriye decode parametre alan decode fonksiyon döndürür"
	[paragraph]
  (let [alfabeA '("b" "c" "d" "e" ) alfabeB '("a" "l" "i" "f" ) allPermutations  ( for [x (permutations alfabeB )] (map list alfabeA x))
        listOfRates (Find-true-permutation_list paragraph allPermutations )  ]
    (cond (= (count listOfRates) 0)(fn [kelime] (Replace-Word kelime  allPermutations ))
          :else (fn [kelime] (Replace-Word kelime (second (nth listOfRates 0 ))))))
)

(defn Gen-Decoder-B-0 "Belirtilen altı harfi alfabe listesinden çıkarır ve kalan alfabeler içinde decode için permutasyon listesi oluşturur. Diğer altı
  harf içinde aynı şekilde permutayon grupları oluşturulur ardından 20 harfli permutasyon listelerine 6 harflı permutasyon listeleri eklenir.
  En son bu permutasyonlar teker teker encoded paragraf üzerinde denenip %50 uyuşmadan büyük en büyük uyuşma sağlayan permutasyon listesi kullanılır.
  Geriye decode yapan bir fonksiyon döndürülür."
	[paragraph]
(let [ rateofList ( sort-by first >  ( remove nil?
                       (set
                         (for  [perm  ( findAllPermutation paragraph) ]
                           (let [sonuc  (Replace-Verify-Paragraph paragraph perm)
                             deneme  (list sonuc)
                              oran (/ (count (filter true? sonuc)) (count paragraph)) ]
                                (cond (>= oran  0.5 ) (list  oran ,perm)  ))))))](cond (not= (count rateofList) 0)
                                                                                       (fn [kelime] (Replace-Word kelime (second (nth rateofList 0))))
                                                                                       :else
                                                                                       (fn[kelime](Replace-Word kelime (list "")  )))
))

(defn Gen-Decoder-B-1
	[paragraph]

)

(defn Code-Breaker
	[document decoder]
 (let [wordDecoder (decoder document)]
   (for [word document](println  "Encoded Word=" word " - Decoded Word =" (wordDecoder word))))

)

;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
	[]
	(let [doc (read-as-list "dictionary1.txt")]
		(println (Code-Breaker doc Gen-Decoder-A))
    (println "------------------------------------")
    (println (Code-Breaker doc Gen-Decoder-B-0))

	)
)
(test_on_test_data)

