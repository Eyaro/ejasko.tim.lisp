
;; customize
(ns ecore.customize)
(def *custom-vars* (ref {}))
(def *groups* (ref {}))

(defn add-group-ns [group ns]
 (dosync (ref-set *groups* (assoc @*groups* group ns))))
 
(defn add-custom-var [group ns var val]
  (let [old (assoc @*custom-vars* group)]
    (dosync
      (ref-set *custom-vars*
        (assoc @*custom-vars* group 
          (assoc old var val)))
      (add-group-ns group ns))))
      
(defn group-ns [group]
  (group @*groups*))

(defn find-custom-var [group varr]
  (varr (group @*custom-vars*)))
(defn find-custom-ns-var [ns varr]
  (find-custom-var (group-ns ns) varr))

(defmacro customize [group varr val]
  `(with-ns (group-ns ~group)
     (dosync
       (add-custom-var ~group ~varr ~val)
       (ref-set ~varr ~val))))

(defmacro defcustom [name str group val]       
 `(dosync
    (defn ~name (ref ~val))
    (add-custom-var ~group *ns* ~name ~val)))
    
    
(defcustom *match*
  "Open/Closed Parentheses finds match"
  SWT/COLOR_DARK_GRAY)
  
       
       