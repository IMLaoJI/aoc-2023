(ns aoc.2023.do
  (:require
    [clojure.core.match :refer [match]]))
(use 'debux.core)
(defmacro return-or [sym expr] {:pre [(symbol? sym)]}
  `(if (reduced? ~sym) (unreduced ~sym) ~expr))

(defn imp-block [[expr & tail-exprs]]
  (dbg(match expr

               (('let pattern init) :seq)
               (if-not tail-exprs
                 (list init)
                 (dbg  (match (imp-block tail-exprs)
                              (((
                                 ((:or 'let `let) bindings & tail) :seq
                                 )
                                ) :seq)
                              `((let [~pattern ~init ~@bindings] ~@tail))
                              block-tail
                              `((let [~pattern ~init] ~@block-tail)))))

               (('let & _) :seq)
               (throw (new IllegalArgumentException
                           "in the root of an `imp` block, `let` must have the form: (let pattern init)"))

               :else
               (if-not tail-exprs
                 (list expr)
                 `(~expr ~@(imp-block tail-exprs))))))

(defn list-to-expr [[expr & tail-exprs]]

  (if-not tail-exprs expr `(do ~expr ~@tail-exprs)))

(defmacro imp
  "Variant of a `do` block where `let` emulates imperative-style variable
  assignment. Convenient for inserting assertions and other side-effectul
  operations between bindings.
  Usage:
    (imp
      (let pattern <expr>)
      (when-not <assertion> (throw <fail>))
      (let pattern <expr>)
      (let pattern <expr>)
      <exprs>
      ...)
  Each `let` creates a subscope. Adjacent lets are merged together.
  Examines only the let expressions at the root level that start with the
  raw 'let symbol. Ignores subforms and 'clojure.core/let."
  [& exprs]
  (list-to-expr (imp-block exprs)))



(defmacro do?
  "Variant of a `do` block with early interruption via clojure.core/reduced.
  When a subform satisfies clojure.core/reduced?, the block short-curcuits,
  immediately returning that value.
  Expansion:
    (do?
      expr  |  (let [value expr] (if (reduced? value) (unreduced value)
      expr  |    (let [value expr] (if (reduced? value) (unreduced value)
      expr  |      (let [value expr] (if (reduced? value) (unreduced value) value)))))))
  "
  [& [expr & tail-exprs]]
  (if tail-exprs
    `(let [value# ~expr]
       (return-or value# (do? ~@tail-exprs)))
    `(unreduced ~expr)))



(defn imp?-expr [exprs]
  (when-let [[expr & tail-exprs] (seq exprs)]
    (match expr

           (('let pattern init) :seq)
           `(let [value# ~init]
              (return-or
                value#
                (let [~pattern value#] ~(imp?-expr tail-exprs))))

           (('let & _) :seq)
           (throw (new IllegalArgumentException
                       "in the root of an `imp?` block, `let` must have the form: (let pattern init)"))

           :else
           (if
             tail-exprs
             `(let [value# ~expr]
                (return-or value# ~(imp?-expr tail-exprs)))
             `(unreduced ~expr)))))

(defmacro imp?

  [& exprs]
  (imp?-expr exprs))

(comment
  (+ 1 1)
  (imp
    (let [one two,three] (range 10))
    (let th (+ three two))
    (+ 1 th)
  )

  ;(macroexpand (imp?
  ;               (let x (+ 1 3))
  ;               (let y (reduced "pre"))
  ;               (when (= y x)  (reduced "success"))
  ;               (+ 3 1)
  ;               ))
  (clojure.pprint/pprint (macroexpand ' (imp
                                          (let [one two,three] (range 10))
                                          (when (= 1 1) 2)
                                          (let th (+ three two))
                                          (+ 1 th)
                                          (+ 3 th))))
  )