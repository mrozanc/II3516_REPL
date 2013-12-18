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
    assign = variableStock space? '=' space? expression
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

(def transform-options
  {:number read-string
   :operator choose-operator
   :operation #(apply %2 [%1 %3])
   :expression identity})

; usage: (interpret (parser "1 + (2 + 3) / 4 - 1 * 2"))
(defn interpret [input]
  (first (->> input (insta/transform transform-options))))

(defn -main  [& args]
  (loop [env {}]
    (let [current-line (read-line)
          current-ast (parser current-line)
          next-env (interpret current-ast)]
      (do (println next-env)
        (recur next-env)))))

;; REPL Read Eval Print Loop
;(loop [env {}]
;  (let [curr-line (read-line)]
;    curr-ast (parser curr-line)
;    next-env (interpret (curr-ast env))
;    (do (println next-env)
;      (recur next-env))))

;(loop [env {}]
;  (let [curr-line (read-line)]))

; ETL: transformation de données
