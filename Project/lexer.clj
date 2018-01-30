(use 'clojure.string)

(defn isAlfabe "" [token] (= (.indexOf
                               (for [i token]
                                 ( re-find #"[a-z]" (str i))) nil) -1)
  )



(defn isint "" [token]
  (= (.indexOf  (for [i (range 0 (- (count token) 1))](let [ temp (nth token i)]

                                                        ( cond (= i 0) (contains? #{"-" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} (str i))
                                                          :else
                                                          (contains? #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} (str temp)) ))) false) -1)

  )

(defn iskeyword "Alınanan kelimenin keyword olup olmadıgını kontrol eder.'" [token]
  (contains?
    #{"and"  "or"  "not" "equal" "append" "concat"
      "set" "deffun" "for" "while"
      "if" "then" "else"} token))

(defn isOpetor "Alınan token'ın operator olup olmamasını inceler " [token]
 (and (not=
    (re-find #"\+|\-|\/|\*|\(|\)" token)
        nil) (= (count token) 1)))

(defn isBinaryvalue "Alınan inputun binany value olup olmamasını kontrol eder" [token] (contains?
                             #{"false" "true"}
                             token))

(defn replaceall "parantezlerin başına bosluk koyar" [input]
  (clojure.string/replace input
                          #"\(|\)"
                          {"(" "( " ")" " )"}))



(defn seperateinput "inputu parçalama işlemi yapar" [input]
  (split (replaceall input) #"\s+"))

(defn lexer [filename](
  let [sepinput (seperateinput (replaceall (str (slurp filename))))]
                     (for [i sepinput] (cond
                                         (iskeyword i) (hash-map "keyword" i)

                                         (isOpetor i) (hash-map "operator" i)

                                         (isint i) (hash-map "integer" i)
                                         (isBinaryvalue i)(hash-map "binaryvalue" i)
                                         (isAlfabe i) (hash-map "id" i)
                                         :else (hash-map  "wronginput" i)))))

 (lexer  "filename")

