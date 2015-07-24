(ns psil-interpreter.core
  (:require [clojure.string :as str]))

(declare evaluate evaluate-parentheses operation)

(defn rrest
  "Gets the rest of the rest of a list
  Returns - (same as given)"
  [list]
  (-> list
      rest
      rest))

(defn remove-edges
  "Removes the first and last characters in a string
  Returns - string"
  [string]
  (-> string
      rest
      butlast
      str/join))

(defn third
  "Gets the third element of a list
  Returns - (same as given)"
  [list]
  (-> list
      rest
      second))

(let [p {\( \)
         \[ \]
         \{ \}}
      a #{\( \) \[ \{ \] \}}]
  (defn balanced?
    "Checks if the parentheses in a string [str] are balanced
  Returns - boolean
  Ref: "
    [s]
    (empty?
     (reduce (fn [[t & b :as stack] s]
               (cond (= (p t) s) b
                     (a s) (conj stack s)
                     :else stack))
             () s))))

(defn return-dict
  "Returns a hashmap of the given parameters [exp] [res] [env]
  Returns - map"
  [exp res env]
  
  {:expressions exp
   :result res
   :environment env})

(defn ret-eval
  "Returns the call of (evaluate (return-dict [][][]))
  Returns - map"
  [exp res env]
  (evaluate (return-dict exp res env)))


(letfn [(inside-operation
          [res operator environment exp-list]
          (if (empty? exp-list)
            res
            (-> (first exp-list)
                (ret-eval nil 
                          environment)
                :result
                (operator res)
                (inside-operation operator environment (rest exp-list)))))]
  (defn operation
    "Either add or multiply [operator] given values using evaluate and the given environment
  Returns - integer"
    [result operator expression-list environment]
    (if (> (count expression-list)
           1)
      (inside-operation result
                        operator
                        environment
                        expression-list)
      (throw (Exception. (str "Invalid Program "))))))


(defn find-open-close-parentheses
  "Looks through a string [str] to find the positions of the innermost set of parentheses
  Returns - vector"
  [str]
  
  (defn helper [string pos1 pos2 count]
    (cond (and (zero? pos1) (not (zero? pos2)))
          (throw (Exception. "Your parentheses don't make sense"))
          
          (and (> pos1 0) (> pos2 0))
          [(dec pos1) (dec pos2)]
          
          (empty? string)
          nil
          
          (= \( (first string))
          (helper (rest string) count pos2 (inc count))
          
          (= \) (first string))
          (helper (rest string) pos1 count (inc count))
          
          :else
          (helper (rest string) pos1 pos2 (inc count))))
  
  (helper str 0 0 1))

(defn get-value
  "Checks if a given expression [exp] maps to a value in [env]
  Returns - integer"
  [exp env]
  
  (if (and (=  (re-find #"[a-zA-Z]+" exp) exp)
           (not (nil? (get env exp))))
    (get env exp)
    (throw (Exception. (str  "Invalid Program " exp env)))))

(defn get-inside-expression
  "Gets the innermost expression in a given expression [exp]
  Returns - string"
  [exp]
  
  (subs
   exp
   (first (find-open-close-parentheses exp))
   (inc (second (find-open-close-parentheses exp)))))



(defn bind
  "Assigns the variable (second [exp-list]) to a value (third [exp-list] and puts this key and value into environment [env]
  Returns - map"
  [exp-list env]
  (let [secexp (second exp-list)]
    (if (=  (re-find #"[a-zA-Z]+" secexp) secexp)
      (conj env
            (hash-map secexp
                      (-> exp-list
                          third
                          (ret-eval nil env)
                          :result)))
      (throw (Exception. (str "Unable to evaluate value: " secexp))))))

(defn evaluate
  "Function that binds the rest together. Takes a map [info] containing the expression to be evaluated, the environment, and a result placeholder.
  Returns - map"
  [info]
  
  (let [untrimmed-exp (get info :expressions)
        exp-list (remove empty? (str/split untrimmed-exp #"\s"))
        exp (str/join " " exp-list)
        fexp (first exp-list)
        env (get info :environment)
        res (get info :result)]
    (cond
     (empty? exp)
     (return-dict exp res env)
     
     (re-find #"\(" exp)
     (evaluate-parentheses exp res env)
     
     (= "+" fexp)
     (return-dict ""
                  (operation 0 + (rest exp-list) env)
                  env)

     (= "*" fexp)
     (return-dict ""
                  (operation 1 * (rest exp-list) env)
                  env)

     (= "bind" fexp)
     (return-dict ""
                  (third exp-list)
                  (bind exp-list env))
     (= (re-find #"[0-9]+" fexp) fexp)
     (return-dict
              ""
              (read-string  fexp)
              env)
     
     :else
     (return-dict "" (get-value fexp env) env))))

(defn evaluate-parentheses
  "Evaluates a given expression [exp] that is within parentheses. Does this by replacing expression within parentheses with the result of evaluating that expression
  Returns - dict"
  [exp res env]
  
  (let [inside-exp (get-inside-expression exp)
        evaluated (-> inside-exp
                      remove-edges
                      (ret-eval nil env))]
    (ret-eval  (->> evaluated
                    :result
                    str
                    (str/replace exp inside-exp))
               nil
               (:environment evaluated))))


(defn make-balanced
  "Finds expressions [exps] which are not complete and combines them into one line"
  [exps]
  
  (cond
   (empty? exps)
   exps
   
   (balanced? (first exps))
   (conj (make-balanced (rest exps)) (first exps))
   
   :else
   (make-balanced (conj
                   (rrest exps)
                   (str/join " " (list  (first exps) (second exps)))))))



(defn separate-line
  "If a line has more than one full expression on it, this function
  separates that line [exps] into individual expressions"
  [exp]
  
  (defn helper
    [exp c]

    (let [current-exp (subs exp 0 c)]
      (cond
       (= c (count exp))
       (list exp)

       (and (re-find #"\(" current-exp)
            (balanced? current-exp))
       (conj (helper
              (subs exp c)
              0)
             current-exp)
       
       :else
       (helper exp (inc c)))))
  (helper exp 1))

(defn evaluate-all
  "Takes a vector of expressions [exps] and recurs through them until
  all have been evaluated. Returns result [res]  of the final line
  Returns - integer"
  [env res exps]
  (if (empty? exps)
    res
    (let [{:keys [result environment expressions]} (-> (first exps)
                                                       (return-dict res env)
                                                       evaluate)]
      (evaluate-all environment result (rest exps)))))

(defn find-errors
  "Finds any errors which have been found to not be caught elsewhere"
  [string]
  
  (cond
   (re-matches #"[0-9]+" string)
   string
  
   (char? string)
   (throw (Exception. "Invalid Program "))

   (re-find #"[^b|+|*|\\(|\\)]" (str (second string)))
   (throw (Exception. "Invalid Program "))

   (re-find #"\(\)" string)
   (throw (Exception. "Invalid Program "))
   
   :else string))

(defn read-psil
  "Reads a text file [psil-file-name] which is assumed to have psil
  and returns result of evaluating all expressions
  Returns - integer"
  [psil-file-name]
  (if (balanced? (slurp psil-file-name))
    (->> psil-file-name
         slurp
         str/split-lines
         (make-balanced)
         (mapcat #(separate-line %))
         (map str/trim)
         (remove empty?)
         (map find-errors)
         (evaluate-all {} nil))
    (throw (Exception. "Invalid Program "))))
