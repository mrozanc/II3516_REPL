(ns parse.core
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "<understandable> = space? baseExpression space?
    <baseExpression> = <'('> space? expression space? <')'> | expression
    <expression> = numberExpression / booleanExpression / dynamicExpression / assign
    numberExpression = number | baseNumberOperation
    booleanExpression = boolean | booleanOperation
    dynamicExpression = variableEval
    number = integer | float
    <integer> = #'\\d+' | <'('> space? #'\\d+' space? <')'>
    <float> = #'\\d+\\.\\d*' | <'('> space? #'\\d+\\.\\d*' space? <')'>
    boolean = 'true' | 'false'
    variableEval = variableName
    variableStock = variableName
    <variableName> = #'[a-zA-Z_]\\w*'
    assign = variableStock space? <'='> space? expression
    <baseNumberOperation> = <'('> space? baseNumberOperation space? <')'> | numberOperation
    numberOperation = (numberExpression | dynamicExpression) space? numberOperator space? (numberExpression | dynamicExpression)
    numberOperator = '*' / '/' / '+' / '-'
    <baseBooleanOperation> = <'('> space? baseBooleanOperation space? <')'> | booleanOperation
    booleanOperation = (booleanExpression / dynamicExpression) space? booleanBinaryOperator space? (booleanExpression / dynamicExpression) | booleanUnaryOperator space? (booleanExpression / dynamicExpression) | (numberExpression | dynamicExpression) space? booleanBinaryOperator space? (numberExpression | dynamicExpression)
    <booleanOperator> = booleanBinaryOperator / booleanUnaryOperator
    booleanBinaryOperator = '==' / '!=' / '<' / '>' / '<=' / '>=' / '&&' / '||'
    booleanUnaryOperator = '!'
    <space> = <#'\\s+'>"))

(defn choose-operator [op]
  (case op
    "+" +
    "-" -
    "*" *
    "/" /
    "==" =
    "!=" not=
    "<" <
    ">" >
    "<=" <=
    ">=" >=
    "&&" (fn [a b] (every? (fn [p] (= true p)) [a b]))
    "||" (fn [a b] (if (= nil (some (fn [p] (= true p)) [a b])) false true))
    "!" not))

(defn prompt []
  (print "% ")
  (flush)
  (read-line))

(defn interpret [input env]
  (let [envOut env
        out (first (->> input (insta/transform {:number #(Long/parseLong %)
                                                :boolean #(case % "true" true "false" false)
                                                :booleanBinaryOperator choose-operator
                                                :booleanUnaryOperator choose-operator
                                                :booleanOperation #(apply %2 [%1 %3])
                                                :numberOperator choose-operator
                                                :numberOperation #(apply %2 [%1 %3])
                                                :variableEval #((keyword (str "var-" %1)) envOut)
                                                :variableStock #(keyword (str "var-" %1))
                                                :assign #(conj envOut {%1 %2})
                                                :numberExpression identity
                                                :booleanExpression identity
                                                :dynamicExpression identity})))]
    (if (= (type out) clojure.lang.PersistentArrayMap)
      {:env out :out nil}
      {:env envOut :out out})
  ))

; usage: (interpret (parser "1 + 1") {:var-b 1})

(defn -main  [& args]
  (loop [env {}]
    (let [current-line (prompt)
          current-ast (parser current-line)
          next-env (interpret current-ast env)]
      (do (println (:out next-env))
        (recur (:env next-env))))))
