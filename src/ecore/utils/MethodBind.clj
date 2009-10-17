(ns ecore.utils.methods (:use ecore.utils.genutils))
(import (java.lang.reflect Modifier Method) (org.eclipse.core.commands ExecutionEvent))


(defn get-methods [cl]
  (map (fn [method] {:method method 
                    :name (.getName method)
                    :return-type (.getReturnType method)
                    :modifiers (Modifier/toString (.getModifiers method))
                    })
                    (.getDeclaredMethods cl)))
                    
(defn get-public-methods [cl & except]                   
  (let [res (filter #(re-find #"public" (:modifiers %)) (get-methods cl))]
    (when except
      (remove #(some #{(:name %)} except) res))))
      

(defn mkseq [item] (if (symbol? item) (seq (list item)) (seq item)))

(defn as-lisp-name [s] 
 (let [matches (mkseq (re-seq #"[a-z]+|[A-Z][a-z]*" s))] 
  (str (apply str (map (fn [m] (str (.toLowerCase m) "-")) (butlast matches))) 
       (.toLowerCase (last matches)))))
       
       
       (map as-lisp-name (map :name (get-public-methods ExecutionEvent "toString")))
