(ns csc344assignment2.core)

(defn andexp [& args] (conj args 'and))
(defn orexp [& args] (conj args 'or))
(defn notexp [e1] (list 'not e1))

(def p1 (andexp 'x(orexp 'x(andexp 'y(notexp 'z)))))
(def p2 (andexp (andexp 'z false)(orexp 'x true)))
(def p3 (orexp true 'a))
(def p4 (andexp 'x 'y 'z))
(def p5 (andexp 'x(orexp 'y 'z)))
(def p6 (notexp (andexp 'x 'y)))

(def arg0)
(def arg1)
(def depth 0)

;Substitutes variables with their bindngs
(defn bind-values [exp bindings]
  (map #(cond
          (seq? %) (bind-values % bindings)
          :else (bindings % %))
       exp))
  
;Simplify boolean expressions
(defn simplify [expvar] 
  (def listsize (count expvar))
  (def depth (inc depth))
  ;(print depth listsize)
  (intern *ns* (symbol (str "op" depth)) (nth expvar 0))
            
  (dotimes [n (- listsize 1)] 
    (cond 
       (seq? (nth expvar (+ n 1))) (intern *ns* (symbol (str "arg" depth n)) (simplify (nth expvar (+ n 1))))
       :default  (intern *ns* (symbol (str "arg" depth n)) (nth expvar (+ n 1))))
    )
  
  (def listsize (count expvar))
  (def op (eval (symbol (str "op" depth))))
  (def arg0 (eval (symbol (str "arg" depth 0))))
  (cond 
      (> listsize 2) (def arg1 (eval (symbol (str "arg" depth 1)))))
  ;(print op depth)
  (cond (> listsize 3) (def input (list op)))
  (cond (> listsize 3) (dotimes [m (- listsize 2)] (def input (conj input (eval (symbol (str "arg" depth (+ m 1))))))))
  (cond (> listsize 3) (def input (list op arg0 (reverse input))))
  
    ;(print arg0 arg1 op listsize)
  
    (def output (cond 
                  (= op 'not) 
                  (cond 
                    (= arg0 'true) 'false
                    (= arg0 'false) 'true
                    (seq? arg0) (cond 
                                  (= 'and (eval (symbol (str "op" (+ depth 1))))) (list 'or (list 'not (eval (symbol (str "arg" (+ depth 1) 0)))) (list 'not (eval (symbol (str "arg" (+ depth 1) 1)))))
                                  (= 'or (eval (symbol (str "op" (+ depth 1))))) (list 'and (list 'not (eval (symbol (str "arg" (+ depth 1) 0)))) (list 'not (eval (symbol (str "arg" (+ depth 1) 1)))))
                                  (= 'not (eval (symbol (str "op" (+ depth 1))))) (eval (symbol (str "arg" (+ depth 1) 0)))
                                  :else (list arg0)
                                  )
                    :else (list 'not arg0)
                    )
    
                  (= op 'and) 
                  (cond
                    (> listsize 3) (simplify input)
                    (= listsize 2) (cond 
                                     (= arg0 'true) 'true
                                     (= arg0 'false) 'false
                                     )
                    (and (= arg0 'true) (= arg1 'true)) 'true
                    (or (= arg0 'false) (= arg1 'false)) 'false
                    (and (= arg0 'true) (and (not= arg1 'true) (not= arg1 'false))) arg1
                    (and (= arg1 'true) (and (not= arg0 'true) (not= arg0 'false))) arg0
                    (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false)) (= arg0 arg1)) arg0
;                    (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false)) (seq? arg1)) arg1
;                    (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false)) (seq? arg0)) arg0
                    (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false))) (list op arg0 arg1)
                    )
    
                  (= op 'or)   
                  (cond
                  (> listsize 3) (simplify input)
                  (= listsize 2) (cond 
                                   (= arg0 'true) 'true
                                   (= arg0 'false) 'false
                                )
                  (and (= arg0 'false) (= arg1 'false)) 'false
                  (or (= arg0 'true) (= arg1 'true)) 'true
                  (and (= arg0 'false) (and (not= arg1 'true) (not= arg1 'false))) arg1
                  (and (= arg1 'false) (and (not= arg0 'true) (not= arg0 'false))) arg0
                  (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false)) (= arg0 arg1)) arg0
;                  (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false)) (seq? arg1)) arg1
;                  (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false)) (seq? arg0)) arg0
                  (and (and (not= arg0 'true) (not= arg0 'false)) (and (not= arg1 'true) (not= arg1 'false))) (list op arg0 arg1)
                  )
                  )
      )
    (def depth (- depth 1))
    ;(print depth output expvar)
    output
    )
  
;The main function
(defn evalexp [exp bindings] 
  (def expvar (bind-values exp bindings))
  (simplify expvar)
  )
  
 