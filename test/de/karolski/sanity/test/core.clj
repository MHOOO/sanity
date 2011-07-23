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
