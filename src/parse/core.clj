(ns parse.core
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "<understandable> = space? baseExpression space?
    <baseExpression> = <'('> space? expression space? <')'> | expression
    expression = numberExpression | assign
    <numberExpression> = number | variableEval | baseOperation
    number = integer | float
    <integer> = #'\\d+' | <'('> space? #'\\d+' space? <')'>
    <float> = #'\\d+\\.\\d*' | <'('> space? #'\\d+\\.\\d*' space? <')'>
    variableEval = variableName
    variableStock = variableName
    <variableName> = #'[a-zA-Z_]\\w*'
    assign = variableStock space? <'='> space? expression
    <baseOperation> = <'('> space? baseOperation space? <')'> | operation
    operation = numberExpression space? operator space? numberExpression
    operator = '*' / '/' / '+' / '-'
    <space> = <#'\\s+'>"))

(defn choose-operator [op]
  (case op
    "+" +
    "-" -
    "*" *
    "/" /))

(let [a {:test 1}
      b 2]
  (conj a {:b b}))

(defn interpret [input env]
  (let [envOut env
        out (first (->> input (insta/transform {:number read-string
                                                :operator choose-operator
                                                :operation #(apply %2 [%1 %3])
                                                :variableEval #((keyword (str "var-" %1)) envOut)
                                                :variableStock #(keyword (str "var-" %1))
                                                :assign #(conj envOut {%1 %2})
                                                :expression identity})))]
    (if (= (type out) clojure.lang.PersistentArrayMap)
      {:env out :out nil}
      {:env envOut :out out})
  ))

; usage: (interpret (parser "1 + 1") {:var-b 1})

(defn -main  [& args]
  (loop [env {}]
    (let [current-line (read-line)
          current-ast (parser current-line)
          next-env (interpret current-ast env)]
      (do (println (:out next-env))
        (recur (:env next-env))))))
