(ns de.karolski.sanity.core
  (:refer-clojure :exclude [reify == inc])
  (use [clojure.pprint :only (cl-format)]
       [clojure.contrib.macro-utils :only (macrolet)]
       (clojure.contrib [logging :only (warn)]
                        [seq-utils :only (positions)])))

(def fstr (partial cl-format nil))

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
               (fstr ~msg (quote ~test) (quote ~val) ~val))))))

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
               (fstr ~msg (:name (meta (resolve (quote ~pred)))) (quote ~val) ~val))))))

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


(defn- check-arglist-for-pluggable
  "Various checks on the arguments to the macro WITH-PLUGGABLE."
  [name plugin-vec]
  ;; ensure the user has specified a symbol for name
  (if (not (symbol? name))
    (throw (Exception. (fstr "WITH-PLUGGABLE expects a SYMBOL
    as its first argument, but got `~S` of type ~A instead."
    name (type name)))))
  
  ;; ensure plugin-list is a vector
  (if (not (vector? plugin-vec))
    (throw (Exception. (fstr "WITH-PLUGGABLE expects a VECTOR
    as its second argument, but got `~S` of type ~A instead."
    plugin-vec (type plugin-vec)))))
  
  ;; ensure the symbol named by NAME is not being used as a function inside plugin-list
  (if-let [code (some
                 #(cond (= name %) %
                        (and (seq? %) (= name (first %))) %)
                 plugin-vec)]
    (throw (Exception. (fstr "Within WITH-PLUGGABLE's second
  argument: PLUGIN-LIST, you may not use the symbol which is named by
  the first argument: NAME. You have used NAME:=~S inside `~S`
  however." name code)))) )

(defn pluggable-macro-body [name args plugin-vec]
  ;; the last element inside the plugin-vec is a symbol
  (let [placer (gensym "placer")]
    ``(do
        ~(def ~placer true)
        (~'~(last plugin-vec)
         ;; any elements before that within the
         ;; plugin-vec are being reduced by applying
         ;; them in-order on the argument list of the
         ;; new macro. This way they can transform the
         ;; arguments however they want.
         ~@(clojure.core/reduce
            (clojure.core/fn
             internal-with-pluggable-fn
             [a# [index# f#]]
             ;; first transform all args according to plugin
             (let [transformed-data#
                   (try
                     (clojure.core/apply f# a#)
                     (catch IllegalArgumentException e#
                       ;; f# must have not enough arguments
                       (-> (cl-format
                            nil
                            "Error at ~A:~A
Plugin #~d to PLUGGABLE `~S` got an invalid number of arguments.
Passed arguments were: '~S.
Either the plugin, or the call to the pluggable has to be fixed."
                            ~(if placer
                               `(if #'~placer
                                  (:file (meta #'~placer))
                                  "unknown")
                               `"unknown")
                            ~(if placer
                               `(if #'~placer
                                  (:line (meta #'~placer))
                                  "unknown")
                               `"unknown")
                            index# '~name (vec a#))
                           (IllegalArgumentException. e#)
                           (throw))))]
               ;; now check whether the plugin did everything the right way
               (cond
                (not (vector? transformed-data#))
                (throw
                 (Exception.
                  (fstr "Plugin #~d of pluggable `~S` did not return a vector."
                        index# '~name)))
                (not (clojure.core/== (count transformed-data#) (count a#)))
                (do (warn
                     (fstr
                      "Plugin #~d of pluggable `~S` returned a vector with ~d instead of ~d elements.
Before: ~S
After: ~S"
                      index# '~name (count transformed-data#) (count a#) a# transformed-data#))
                    transformed-data#)
                ;; for easier debugging, plugins should
                ;; preserve types - but this does not always
                ;; have to be the case, so we only warn on it
                (not (clojure.core/=
                      (clojure.core/map-indexed
                       (clojure.core/fn [i# e#] [i# (clojure.core/type e#)]) a#)
                      (clojure.core/map-indexed
                       (clojure.core/fn [i# e#] [i# (clojure.core/type e#)]) transformed-data#)))
                (do
                  (warn
                   (fstr
                    "In ns ~S: Plugin #~d of pluggable `~S` returned a
                           vector with differing types. It should be ~S, but got ~S instead."
                    (ns-name *ns*) index# '~name (map type a#) (map type transformed-data#)))
                  transformed-data#)
                :else transformed-data#)
               ))
            ~args
            ~(clojure.core/vec
              ;; index the elements, so we can use indices as
              ;; hints inside error messages
              (map-indexed (comp vec list)
                           (clojure.core/butlast
                            plugin-vec))))))))

(defmacro with-pluggable
  "Macro which defines a new macro within its body with the specified
  NAME. The second argument is a vector of N elements, where the first
  N-1 elements are argument transformer functions & the N'th element
  is a base macro which will finally be called with the transformed
  arguments. The argument transformer functions must all take the same
  arguments as the base macro. They may change the arguments in any
  way, but must return a list of the (possibly transformed) arguments
  when they're done.

  Note that the plugins may not capture the local context (no closures
  per (fn ...) directly inside PLUGIN-VEC). They may however be
  globally defined closures (i.e. closures per (defn ...)).

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
  [name plugin-vec & body]
  (check-arglist-for-pluggable name plugin-vec)
  (let [args (gensym "args")]
    `(macrolet
       [(~name [& ~args]
               ~(pluggable-macro-body name args plugin-vec))]
       ~@body)))

(defmacro defpluggable
  "Like WITH-PLUGGABLE, but on a namespace scale by defining a new
  macro through DEFMACRO (instead of MACROLET)."
  [name plugin-vec]
  (check-arglist-for-pluggable name plugin-vec)
  (let [args (gensym "args")
        placer (gensym "placer")]
    `(do
       ;; (def ~placer true)
       (defmacro
         ~name
         [& ~args]
         ~(pluggable-macro-body name args plugin-vec ;; :placer placer
                                )))))

(defn- add-metadata-on-arglist-using-deduction-map [arglist deduce-map]
  (-> (fn [sym]
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
      (vec)))

(defn validate-arg-type-from-meta
  "Given a symbol SYM containing :type metadata and a value ARG, check
  whether ARG is an instance of (:type (meta SYM)). If not, throw an
  IllegalArgumentException."
  [sym arg]
  (if (not (and (not (nil? arg))
                (instance? (:type (meta sym)) arg)))
    (throw
     (IllegalArgumentException.
      (fstr "Expected argument `~A` to be of type `~S` but got type `~S` instead." sym (:type (meta sym)) (type arg))))))

(defn argument-type-deducer-plugin
  "Plugin for with-pluggable. Returns a function which transforms the
  argument list by adding :type metadata to symbols which match
  regular expressions inside (optional) kw arg DEDUCE-MAP. It does so
  according to the with-pluggable specs when used on a defn form of
  structure \"(defn NAME ARGLIST BODY)\" (note: no multiple
  arity dispatch implemented as of now).

  The DEDUCE-MAP must be a map of the form {R_1 T_1, R_2 T_2, ..., R_n
  T_n}, n \\in [0,inf]. With R_i being regular expressions and T_i any
  type/class for i \\in [0,n]. "
  [& {:keys [deduce-map]}]
  (fn argument-type-deducer-fn
    [& args]
    (if-let [arglist (first (filter vector? args))]
      (vec (replace {arglist (add-metadata-on-arglist-using-deduction-map arglist deduce-map)}
                    args))
      ;; TODO: arity dispatch
      (vec args))))

(defn argument-type-assertion-plugin
  "Return a function which takes three arguments (NAME, ARGLIST &
  BODY) just like a DEFN form (without docstring) and adds assertions
  for any type hinted symbol argument to the BODY argument."
  []
  (fn argument-type-assertion-fn 
    ([& args]
       (if-let [arglist (first (filter vector? args))]
         (let [arglist-position (first (positions #{arglist} args))
               prepost-conditions? (map? (nth args (+ 1 arglist-position)))
               first-body-position (+ arglist-position (if prepost-conditions? 2 1))
               assertable-args (filter #(:type (meta %)) arglist)
               assertion-body
               (map (fn argument-type-assertion-builder-fn [arg]
                      `(validate-arg-type-from-meta '~arg ~arg))
                    assertable-args)]
           (vec (concat (take first-body-position args) (concat assertion-body (drop first-body-position args)))))
         ;; TODO: arity dispatch
         (vec args)))))