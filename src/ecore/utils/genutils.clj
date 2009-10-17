(ns ecore.utils.genutils
  ;(:require )
  ;(:use )
  ;(:import )
  )

(defmacro prog1 [& body]
  " (prog1 forms*)
Evaluates all the forms, returning the result of the first form"
  `(let [result# ~(first body)]
     ~@(rest body)
     result#))

(defn jprint [item]
  (.print System/out item))
(defn jprintln [item]
  (.println System/out item))
  

(defmacro defcustom [name str init]
  `(def ~name ~init))

(defn str= [#^String str1 #^String str2] (.equals str1 str2))
(defn string-equals [#^String str1 #^String str2] (.equalsIgnoreCase str1 str2))

(defmacro cond* [& cond]
  (let [bindings
        (mapcat
          #(list (first %) `(do ~@(rest %)))
            cond)] (println bindings)
    `(cond ~@bindings)))

(defn mklst [seq] (if (symbol? seq) (list seq) seq))

(defmacro defnop [sym args & body]
  (let [pos-ops (split-with (complement #(= % '&op)) args)
        ps (pos-ops 0)
        ops (rest (pos-ops 1))
        op-default-vals (reverse (map #(second (mklst %)) ops))
        op-names (map #(first (mklst %)) ops)
        func-args (map #(let [func-args `(~@ps ~@(take % op-names))
                              default-args (reverse (take (- (count ops) %) op-default-vals))]
                          (list
                            (vec func-args)
                            `(~sym ~@func-args ~@default-args)))
                    (range 0 (count ops)))]
    `(defn ~sym 
       ([~@ps ~@op-names] ~@body)
       ~@func-args)))
    
(defmacro defnk [sym args & body]
  (let [pos-keys (split-with (complement keyword?) args)
        ps (pos-keys 0)
        ks (apply array-map (pos-keys 1))
        gkeys (gensym "gkeys__")
        letk (fn [ke]
               (let [k (key ke)
                     kname (symbol (name k))
                     v (val ke)]
                 `(~kname (if (contains? ~gkeys ~k) (~gkeys ~k) ~v))))]
    `(defn ~sym [~@ps & k#]
       (let [~gkeys (apply hash-map k#)
             ~@(apply concat (map letk ks))]
         ~@body))))

(defmacro catch-if [test ret & body]
  `(try
     (do ~@body)
     (catch java.lang.Exception e# (if ~test (throw e#) ~ret))))

(defmacro aif [test & [then else]]
  `(let [~'it ~test]
     (if ~'it ~then ~else)))
     
(defmacro awhen [test & body]
  `(aif ~test (do ~@body)))
  
  
(defn symkey [sym] (read-string (str ":" sym)))

(defnop sym-seq [sym start &op end]
  (if end
    (read-string (subs (str sym) start end))
    (read-string (subs (str sym) start))))
(defn sym-at [sym num] (sym-seq sym num (inc num)))
  

;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

(defmacro abbrev [short long]
  `(defmacro ~short [& args#]
     `(~'~long ~@args#)))
(defmacro abbrevs [& names]
  `(do ~@(map (fn [pair] `(abbrev ~@pair)) names)))


;;copied because i don't know where it is in my jar
(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
one at a time. If a test returns logical true, cond-let evaluates and
returns expr with binding-form bound to the value of test and doesn't
evaluate any of the other tests or exprs. To provide a default value
either provide a literal that evaluates to logical true and is
binding-compatible with binding-form, or use :else as the test and don't
refer to any parts of binding-form in the expr. (cond-let binding-form)
returns nil."
  [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))

(defmacro acond [& binds]
  (let [new-binds (mapcat (fn [b] `(~(first b) (do ~@(rest b)))) binds)]
    `(cond-let (~'it) ~@new-binds)))

(defn aseq? [item] (or (vector? item) (list? item) (seq? item)))

(defn mkseq [item]
  (cond
    (symbol? item) (seq (list item))
    :default (seq item)))

(defnop lassoc [item lst &op (test =)]
  (first (filter #(test (first (mkseq %)) item) lst)))

(defmacro llet [bind & body] 
  `(let ~(vec 
           (mapcat #(if (symbol? %) (list % nil) %) bind))
     ~@body))

(defmacro lcond [& cond]
  (let [bindings
        (mapcat
          #(list (first %) `(do ~@(rest %)))
          cond)]
    `(cond ~@bindings)))
;;;;;;
;;;;;;
;;;;;;


(defn condlet-binds [vars cl]
  (map (fn [bindform]
         (if (list? bindform)
           (cons
             (second (lassoc (first bindform) vars))
             (rest bindform))))
    (rest cl)))

(defn condlet-clauses [vars cl bodfn]
  `(~(first cl) (llet ~(map second vars)
                  (llet ~(condlet-binds vars cl)
                    (~bodfn ~@(map second vars))))))

(defmacro condlet [clauses & body]
  (let [vars (map #(list % (gensym))
               (distinct
                 (map first
                   (mapcat rest clauses))))
        bodfn (gensym)]
    `(letfn [(~bodfn ~(vec (map first vars))
               ~@body)]
       (lcond ~@(map #(condlet-clauses vars  % bodfn)
                  clauses)))))

(defmacro acondlet [clauses & body]
  (let [vars (map #(list % (gensym))
               (distinct
                 (map first
                   (mapcat rest clauses))))
        bodfn (gensym)]
    `(letfn ((~bodfn ~(vec (map first vars))
               ~@body))
       (acond ~@(map #(condlet-clauses vars  % bodfn)
                  clauses)))))