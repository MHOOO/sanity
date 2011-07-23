(ns de.karolski.sanity.core
  (:refer-clojure :exclude [reify == inc])
  (use [clojure.pprint :only (cl-format)]
       (clojure.core.logic [prelude :only (matche)]
                           minikanren)))

(def ^:private str-map
     {:test-failed "Test failed: ~A\n for ~A = ~A"
      :test-failed-on-pred "Test failed: (~A ~A~:*)\n for ~A = ~A"})

(defn- failure-msg
  "Return the cl-format ready string for the test given the test failure type."
  [kw]
  (kw str-map))


(defmacro assert-with-msg [val test msg]
  `(let [result# ~test]
     (when (not result#)
       (throw (Exception.
               (cl-format nil ~msg (quote ~test) (quote ~val) ~val))))))

(defmacro assert-with-msg-on-pred
  "Evaluate (pred val). If that returns nil/false, throw an Exception
  with the specified message. The message will be passed to cl-format
  with these additional arguments (in that order):
   - un-evaluated val
   - un-evaluated pred
   - evaluated val"
  [val pred msg]
  `(let [result# ~(pred val)]
     (when (not result#)
       (throw (Exception.
               (cl-format nil ~msg (:name (meta (resolve (quote ~pred)))) (quote ~val) ~val))))))

(defmacro assert* [val test]
  `(assert-with-msg ~val ~test (failure-msg :test-failed)))

(defmulti validate* (fn [val test] test))

(defmethod validate* :non-zero [x _]
  (assert* x (not= x 0)))

(defmethod validate* :even [x _]
  (assert* x (even? x)))

(defmethod validate* :default [x f]
  (assert-with-msg-on-pred x f (failure-msg :test-failed-on-pred)))

(defn validate [& tests]
  (doseq [test tests] (apply validate* test)))


;; Example:
(comment
  (defn divide [x y]
    (validate [y number?])
    (/ x y)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVEN BETTER enforcing parameters by deduction
(comment
  ;; consider this fn 
  (defn do-something-on-entity [entity]
    (assoc entity :name "Some Entity"))
  ;; it obviously expects an entity as its first parameter now, lets
  ;; say you only have *one* kind of thing, which can be an entity: An
  ;; `Entity` record.
  ;;
  ;; It would be nice, if defn could deduce from its only argument:
  ;;   - that the argument named "entity" is of *type* Entity
  ;; And furthermore if defn could then:
  ;;   - add a type hint to the "entity" argument
  ;;   - validate whether the "entity" argument is actually of type Entity during run-time
  ;;   
  ;; These two steps should however not be the default behavior. We need a
  ;; mechanism for activating them whenever a user has the need for
  ;; them.
  ;; If defn were a macro, which allowed "plug-ins" to change both
  ;; parameter list & body. Plus if we could enable "plug-ins" on a
  ;; local scope. Then we'd have everything in place, to build such a
  ;; system.


  (defmulti validate-argument-by-type (comp type second list))
  
  ;; validation of arguments
  (defmethod validate-argument-by-type Entity [v t]
    (validate [v entity?]))

  ;; global deducing of type from a symbol
  (defmethod deduce-argument-type-from-symbol 'entity [t]
    Entity)

  ;; namespace-only deducing of type from a symbol
  (defmethod deduce-argument-type-from-symbol-inside-ns 'entity [ns t]
    Entity)


  (plug-into defn
   [;; adds type hints to argument metadata
    (argument-type-deducer-plugin
     ;; locally scoped deduce-map
     :deduce-map {'entity Entity})

    ;; adds validations for typed arguments
    (argument-type-validator-plugin)

    ;; theoretically we could also add in other plugins although in
    ;; this case matchure is probably not compatible with the other
    ;; plugins (or the other way around)
    (matchure-plugin)
    ]
   
   (defn do-something-on-entity [entity]
     (assoc entity :name "Some Entity")))

  ;; given above definitions, this would yield the following "true" defn
  (defn do-something-on-entity [^Entity entity]
    (validate-argument-by-type Entity entity)
    (assoc entity :name "Some Entity"))

  ;; now, instead of having to specify these things all the time,
  ;; define a new macro for that purpose
  (define-pluggable-macro
    defn-pluggable
    ;; plugins
    [(argument-type-deducer-plugin
      ;; map x & y to integers
      :deduce-map {'x Integer 'y Integer})
     (argument-type-validator-fn)]
    ;; arguments
    [& body]
    `(defn ~@body))

  ;; now we can do
  (defn-pluggable add [x y] (+ x y))

  ;; x is not an integer, but a double instead? Set it yourself to override.
  (defn-pluggable add [^double x ^double y] (+ x y))

  ;; or update values locally
  (override-plugins
   [(argument-type-deducer-plugin
     ;; map x & y to double
     :deduce-map {'x Double 'y Double})]

   (defn-pluggable add [x y] (+ x y)))

  ;; or even
  (plug-into defn-pluggable
   [(some-other-plugin)]
   (defn-pluggable add [x y] (+ x y)))

  ;; PROBLEM: How could one change the arguments on an already plugged in plugin?
  ;; ANSWER:
  ;;   Use (plug-into) with the same plugin but other args again.
  ;;   Or use (override-plugins).

  ;; PROBLEM: argument-type-deducer & matchure plugin are incompatible. Why?
  ;; ANSWER:
  ;; 
  ;;   Deducing argument types is done by an algorithm, which looks at
  ;;   the name of any argument and then looks up the type based on
  ;;   that particular name. The algorithm must know what arguments
  ;;   there are. Since matchure uses its own argument list format,
  ;;   there is no way for the default deduce algorithm to figure out
  ;;   what inside the argument list is an argument and what not. This
  ;;   is why they are incompatible.
  ;;
  ;; SOLUTION:
  ;;
  ;;   Every function has an argument list and a body to operate
  ;;   on. These are the two elements any plugin operates on. Instead
  ;;   of passing in the "raw" argument list to each plugin (i.e. the
  ;;   argument list, as it is typed in by the programmer) we pass the
  ;;   argument list in a standard format.
  ;;
  ;;   Every plugin has the ability to operate on that standard format
  ;;   and make modifications (e.g. by adding meta-data to the
  ;;   symbols) to it. A plugin which has a new argument list format
  ;;   can provide a fn to map the argument list into the standard
  ;;   format. This standard format is then what is passed to any
  ;;   other plugin.
  ;;
  ;;   Consider for example the matchure arglist:
  ;;     A := [(and ?e (entity? ?)) ?entity]
  ;;     
  ;;   The matchure plug-in must provide for an argument mapping
  ;;   function which transforms A into the following:
  ;;     B := [entity]
  ;;   In the case of the above, based on the predicate entity? it may even be:
  ;;     B := [^Entity entity]
  ;;
  ;;   Validation will at this point no longer be necessary, as
  ;;   matchure has validated the argument already.
  ;;
  ;; PROBLEM: Do we have to transform the body when transforming the arglist?
  ;; ANSWER:
  ;; 
  ;;   I don't think so. Arguments in the matchure arglist are
  ;;   prepended with a question mark. Within the body they are mapped
  ;;   to their non-question-mark counterparts (i.e. "?entity" ->
  ;;   "entity").  As such every other plug-in may work on those
  ;;   arguments, as long as the standard argument map lists them.

  )



(defrecord Ship [])

(defmulti deduce-argument-type-from-symbol
  ;; Our custom dispatch function
  (fn [sym]
    ;; we take all of the defined methods and look at which dispatch
    ;; value (a regex) matches the symbol name.
    (ffirst
     (filter
      (fn [[regex method-fn]]
        (re-matches regex (name sym)))
      (methods deduce-argument-type-from-symbol)))
    ))

(defmethod deduce-argument-type-from-symbol #"ship" [_]
  Ship)

(defmethod deduce-argument-type-from-symbol #"^[a-z]+-count" [_]
  Long)

(defn ns-of
  "Return the namespace object of a function object by looking at the
  stringified function name."
  [f]
  (-> #"^([^$]+)"
      (re-find (str f))
      (first )
      (symbol )
      (find-ns )))

(defmulti deduce-argument-type-from-symbol-on-ns
  (fn [ns sym]
    ;; we take all of the defined methods and look at which dispatch
    ;; value (a regex) matches the symbol name.
    (ffirst
     (filter
      (fn [[regex method-fn]]
        (and (= (ns-of method-fn) ns) (re-matches regex (name sym))))
      (methods deduce-argument-type-from-symbol-on-ns)))))

(defrecord Ship [])
(defmethod deduce-argument-type-from-symbol-on-ns #"ship" [_ _]
  Ship)

(defmethod deduce-argument-type-from-symbol-on-ns #"image-count" [_ _]
  Long)

(use '[clojure.contrib.macro-utils :only (macrolet)])
(defmacro with-pluggable
  "Macro which defines a new macro within its body with the specified
  NAME. The second argument is a vector of N elements, where the first
  N-1 elements are argument transformer functions & the N'th element
  is a base macro which will finally be called with the transformed
  arguments. The argument transformer functions must all take the same
  arguments as the base macro. They may change the arguments in any
  way, but must return a list of the (possibly transformed) arguments
  when they're done.

  Example:
  (with-pluggable 
          defn
          [;; a custom function which transforms the body
           (fn [name arglist body] 
               [name 
                arglist 
                ;; Wrap the body of any function inside a:
                ;; (do (println \"...\") <original body>)
                `(do 
                  (println \"called pluggable with arguments:\" ~@arglist) 
                  ~body)])

           ;; the base macro is the function definition macro: defn
           clojure.core/defn]

        ;; by using the custom defn,
        ;; my-identity will now have a transformed body.
        (defn my-identity [x] x))


  ;; Try it!
  (my-identity 1)"
  [name plugin-list & body]
  (let [args (gensym "args")]
    `(macrolet [(~name [& ~args]       ;;
                       ;; the last element inside the plugin-list is a symbol
                       `(~'~(last plugin-list)
                         ;; any elements before that within the
                         ;; plugin-list are being reduced by applying
                         ;; them in-order on the argument list of the
                         ;; new macro. This way they can transform the
                         ;; arguments however they want.
                          ~@(reduce (fn [a# f#] (apply f# a#))
                                    ~args ~(vec (butlast plugin-list)))))]
       ~@body)))

(defn argument-type-deducer-plugin
  "Plugin for with-pluggable. Returns a function which transforms the
  argument list by adding :type metadata to symbols which match
  regular expressions inside (optional) kw arg DEDUCE-MAP. It does so
  according to the with-pluggable specs when used on a defn form of
  structure \"(defn NAME ARGLIST BODY)\" (note: no docstring/multiple
  bodies possible as of now).

  The DEDUCE-MAP must be a map of the form {R_1 T_1, R_2 T_2, ..., R_n
  T_n}, n \\in [0,inf]. With R_i being regular expressions and T_i any
  type/class for i \\in [0,n]. "
  [& {:keys [deduce-map]}]
  (fn [name arglist body]
    (let [arglist (-> (fn [sym]
                        (if-let [deduced-type
                                 (some
                                  (fn [[regex t]] 
                                    (if (re-matches regex (str sym))
                                      t))
                                  deduce-map)]
                          (vary-meta
                           sym
                           (fn [m] (merge m {:type deduced-type})))
                          sym))
                      (map arglist)
                      (vec))]
      [name arglist body])))

(defn argument-type-assertion-plugin
  "Return a function which takes three arguments (NAME, ARGLIST &
  BODY) just like a DEFN form (without docstring) and adds assertions
  for any type hinted symbol argument to the BODY argument."
  []
  (fn [name arglist body]
    (let [assertable-args (filter #(:type (meta %)) arglist)
          assertion-body
          (map #(do `(assert
                      (and (not (nil? ~%))
                           (instance? ~(:type (meta %)) ~%))))
               assertable-args)] 
      [name arglist `(do
                       ~@assertion-body
                       ~body)])))