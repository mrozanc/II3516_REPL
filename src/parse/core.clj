(ns parse.core
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "<understandable>       = space? (function / instruction) space? / baseExpression
    functionCall           = <'('> space? variableEval (space? expression)* space? <')'>
    function               = <'('> space? <'['> space? arguments space? <']'> space functionBody space? <')'>
    functionBody           = #'[^\\(\\)]*(\\([^\\(\\)]*?\\))*[^\\(\\)]*?'
    arguments              = argument? (space argument)*
    <instruction>          = space? (ifExpression / whileExpression / baseExpression) space?
    <baseExpression>       = <'('> space? expression space? <')'> / expression
    <expression>           = ternaryExpression / numberExpression / booleanExpression / dynamicExpression / assign
    ifExpression           = <'if'> space? <'('> space? booleanExpression space? <')'> space? block space? (<'else'> space? block)?
    ternaryExpression      = booleanExpression space? <'?'> space? baseExpression space? <':'> space? baseExpression
    whileExpression        = <'while'> space? <'('> space? whileCondition space? <')'> space? <'{'> space? whileBody space? <'}'>
    whileCondition         = #'[^\\(\\)]*(\\([^\\(\\)]*?\\))*[^\\(\\)]*?'
    whileBody              = #'[^\\{\\}]*(\\{[^\\{\\}]*?\\})*[^\\{\\}]*?'
    block                  = <'{'> space? instruction* space? <'}'>
    numberExpression       = number | baseNumberOperation
    booleanExpression      = boolean | booleanOperation
    dynamicExpression      = functionCall | variableEval
    number                 = integer | float
    <integer>              = #'\\d+' | <'('> space? #'\\d+' space? <')'>
    <float>                = #'\\d+\\.\\d*' | <'('> space? #'\\d+\\.\\d*' space? <')'>
    boolean                = 'true' | 'false'
    argument               = variableName
    variableEval           = variableName
    variableStock          = variableName
    <variableName>         = #'[a-zA-Z_]\\w*'
    assign                 = variableStock space? <'='> space? (expression | function space?)
    <baseNumberOperation>  = <'('> space? baseNumberOperation space? <')'> | numberOperation
    numberOperation        = (numberExpression | dynamicExpression) space? numberOperator space? (numberExpression | dynamicExpression)
    numberOperator         = '*' / '/' / '+' / '-'
    <baseBooleanOperation> = <'('> space? baseBooleanOperation space? <')'> | booleanOperation
    booleanOperation       = (booleanExpression / dynamicExpression) space? booleanBinaryOperator space? (booleanExpression / dynamicExpression)
                             | booleanUnaryOperator space? (booleanExpression / dynamicExpression)
                             | (numberExpression / dynamicExpression) space? booleanBinaryOperator space? (numberExpression / dynamicExpression)
    <booleanOperator>      = booleanBinaryOperator / booleanUnaryOperator
    booleanBinaryOperator  = '==' / '!=' / '<' / '>' / '<=' / '>=' / '&&' / '||'
    booleanUnaryOperator   = '!'
    <space>                = <#'[\\s\\n]+'>"))

(defn choose-operator [op]
  (case op
    "+"  +
    "-"  -
    "*"  *
    "/"  /
    "==" =
    "!=" not=
    "<"  <
    ">"  >
    "<=" <=
    ">=" >=
    "&&" (fn [a b] (every? (fn [p] (= true p)) [a b]))
    "||" (fn [a b] (if (= nil (some (fn [p] (= true p)) [a b])) false true))
    "!"  not))

(defn prompt []
  (print "% ")
  (flush)
  (read-line))

(defn interpret [input env]
  (let [envOut env
        config {:function list
                :functionBody str
                :functionCall (fn
                                ([function] (interpret (parser (first (rest function))) {}))
                                ([function & args] (:out (interpret (parser (first (rest function))) (loop [fenv {}
                                                                                                            vars (first function)
                                                                                                            arguments args]
                                                                                                       (let [var (first vars)
                                                                                                             fenv (conj fenv {(keyword (str "var-" var)) (first arguments)})
                                                                                                             vars (rest vars)
                                                                                                             arguments (rest arguments)]
                                                                                                         (if (not (empty? vars))
                                                                                                           (recur fenv vars arguments)
                                                                                                           fenv)))))))
                :arguments list
                :argument str
                :block (fn [& instructions] (map identity instructions))
                :number #(Long/parseLong %)
                :boolean #(case % "true" true "false" false)
                :booleanBinaryOperator choose-operator
                :booleanUnaryOperator choose-operator
                :booleanOperation (fn ([o a] (apply o [a])) ([a o b] (apply o [a b])))
                :numberOperator choose-operator
                :numberOperation #(apply %2 [%1 %3])
                :variableEval #((keyword (str "var-" %1)) envOut)
                :variableStock #(keyword (str "var-" %1))
                :assign #(conj envOut {%1 %2})
                :numberExpression identity
                :booleanExpression identity
                :dynamicExpression identity
                :ifExpression (fn ([c t] (if c t)) ([c t e] (if c t e)))
                :ternaryExpression (fn [c t e] (if c t e))
                :whileCondition str
                :whileBody str
                :whileExpression (fn [c b] (let [c-ast (parser c)
                                                 b-ast (parser b)]
                                             (loop [wenv env]
                                               (if (:out (interpret c-ast wenv))
                                                 (let [wenv (:env (interpret b-ast wenv))]
                                                   (recur wenv))
                                                 wenv))))}
        out (first (->> input (insta/transform config)))]
    (let [realOut (if (= (type out) clojure.lang.PersistentArrayMap)
        {:env out :out nil}
        {:env envOut :out out})]
      (if (not (empty? (:nextInstr out)))
        (interpret (parser (:nextInstr out)) realOut)
        realOut))
  ))

; usage: (interpret (parser "1 + 1") {:var-b 1})

(defn -main  [& args]
  (loop [env {}]
    (let [current-line (prompt)
          current-ast (parser current-line)
          next-env (interpret current-ast env)]
      (do (println (:out next-env))
        (recur (:env next-env))))))
