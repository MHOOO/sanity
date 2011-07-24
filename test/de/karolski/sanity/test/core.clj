(ns de.karolski.sanity.test.core
  (:use
   [de.karolski.sanity.core]
   [lazytest.describe :only (describe it testing)]
   [lazytest.expect :only (expect)]))

(describe with-pluggable
  (it "Should not change the function in any way."
    (expect
     (== ((with-pluggable
            fn+
            [(fn [arglist body]
               [arglist body])
             clojure.core/fn]
            (fn+ [] 1)))
         1)))
  (it "Should not change the function in any way."
    (expect
     (== ((with-pluggable
            fn+
            [(fn [arglist body]
               [arglist body])
             clojure.core/fn]
            (fn+ [x] x)) 1)
         1)))

  (it "Should replace the function arglist."
    (expect
     (== ((with-pluggable
            fn+
            [(fn [arglist body]
               ['[x y] body])
             clojure.core/fn]
            (fn+ [x] y)) 1 2)
         2)))
  (it "Should add another argument to the function arglist."
    (expect
     (== ((with-pluggable
            fn+
            [(fn [arglist body]
               [(vec (concat arglist '[y])) body])
             clojure.core/fn]
            (fn+ [x] y)) 1 2)
         2)))
  (it "Should replace the function body."
    (expect
     (== ((with-pluggable
            fn+
            [(fn [arglist body]
               [arglist 1])
             clojure.core/fn]
            (fn+ [x] x)) 100)
         1))))



(describe defpluggable
  (it "Should not change the function in any way."
    (defpluggable
      fn+
      [(fn [arglist body]
         [arglist body])
       clojure.core/fn])
    (expect (== (fn+ [] 1) 1)))
  (it "Should not change the function in any way."
    (defpluggable
      fn+
      [(fn [arglist body]
         [arglist body])
       clojure.core/fn])
    (expect
     (== ((fn+ [x] x) 1) 1)))

  (it "Should replace the function arglist."
    (defpluggable
      fn+
      [(fn [arglist body]
         ['[x y] body])
       clojure.core/fn])
    (expect
     (== ((fn+ [x] y) 1 2)
         2)))
  (it "Should add another argument to the function arglist."
    (defpluggable
      fn+
      [(fn [arglist body]
         [(vec (concat arglist '[y])) body])
       clojure.core/fn])
    (expect
     (== ((fn+ [x] y) 1 2) 2)))
  (it "Should replace the function body."
    (defpluggable
      fn+
      [(fn [arglist body]
         [arglist 1])
       clojure.core/fn])
    (expect
     (== ((fn+ [x] x) 100)
         1))))


(describe argument-type-deducer-plugin
  (it "Should not do anything."
    (expect (== ((with-pluggable fn+
                   [(argument-type-deducer-plugin)
                    clojure.core/fn]
                   (fn+ [x] x)) 100)
                100)))
  (it "Should not do anything on non-matched arguments."
    (expect (== ((with-pluggable fn+
                   [(argument-type-deducer-plugin :deduce-map {#"i" Integer})
                    clojure.core/fn]
                   (fn+ [x] x)) 100)
                100)))

  (it "Should deduce deduce type on matched argument."
    (expect (= ((with-pluggable fn+
                  ;;NOTE: If this fails, try with Long instead (clojure 1.3 uses long)
                  [(argument-type-deducer-plugin :deduce-map {#"x" Integer})
                   (fn [arglist body] 
                     [arglist `(list ~@(map meta arglist))])
                   clojure.core/fn]
                  (fn+ [x] nil)) 1)
                (list {:type java.lang.Integer}))))
  (it "Should deduce deduce type correctly on matched arguments and leave non-matched arguments alone."
    (expect (= ((with-pluggable fn+
                  [(argument-type-deducer-plugin :deduce-map {#"x" Integer #".*-count" Integer})
                   (fn [arglist body] 
                     [arglist `(list ~@(map meta arglist))])
                   clojure.core/fn]
                  (fn+ [a x b arg-count] nil)) 1 2 3 4)
                (list nil {:type java.lang.Integer} nil {:type java.lang.Integer}))))

  (it "Should not change anything on multi arity dispatch."
    (expect (= ((with-pluggable fn+
                  [(argument-type-deducer-plugin) 
                   clojure.core/fn]
                  (fn+ ([x] nil) ([x y] nil))) 1))))
  )

(describe argument-type-assertion-plugin
  (it "Should not change anything"
    (expect (= ((argument-type-assertion-plugin) 'foo ['x] 'x)
               '(foo [x] x))))
  (it "Should add assertion code"
    (expect (= ((argument-type-assertion-plugin) 'foo [(with-meta 'x {:type Long})] 'x)
               (let [x (with-meta 'x {:type Long})]
                 `[~'foo [~x] (validate-arg-type-from-meta '~x ~x) ~'x]))))
  (it "Should add assertion code even when using :pre & :postconditions"
    (expect (= ((argument-type-assertion-plugin) 'foo [(with-meta 'x {:type Long})] '{:pre [(number? x)]} 'x)
               (let [x (with-meta 'x {:type Long})]
                `[~'foo [~x]
                  {:pre [(~'number? ~'x)]}
                  (validate-arg-type-from-meta '~x ~x) ~'x]))))
  (it "Should also work correctly with WITH-PLUGGABLE"
    (expect
     (== ((with-pluggable
            fn+
            [(argument-type-deducer-plugin :deduce-map {#"x" Integer})
             (argument-type-assertion-plugin)
             clojure.core/fn]
            (fn+ foo [x] {:pre [(number? x)] :post [(number? %)]} x))
          100)
         100)))
  (it "Should also work correctly with WITH-PLUGGABLE and docstrings."
    (expect
     (== ((with-pluggable
            defn
            [(argument-type-deducer-plugin :deduce-map {#"x" Integer})
             (argument-type-assertion-plugin)
             clojure.core/defn]
            (defn foo "Identity with number checking." [x] {:pre [(number? x)] :post [(number? %)]} x))
          100)
         100)))
  (it "Should not change anything with WITH-PLUGGABLE and multi arity dispatch."
    (expect
     (== ((with-pluggable
            defn
            [(argument-type-deducer-plugin :deduce-map {#"x" Integer})
             (argument-type-assertion-plugin)
             clojure.core/defn]
            (defn foo 
              "Identity with number checking."
              ([x] {:pre [(number? x)] :post [(number? %)]} x)
              ([x y] {:pre [(number? x)] :post [(number? %)]} x)))
          100)
         100))))