;;;
(ns ecore.env
 (:use ecore.utils.genutils clojure.contrib.with-ns)
 (:import
   (org.eclipse.ui 
     PlatformUI IWorkbench 
     IWorkbenchWindow IWorkbenchPage)
   (jasko.tim.lisp.editors LispEditor)))

(def *env* (atom {}))

(defn env [] *env*)
(defn get-env [] (deref *env*))
(defn set-env [to] (reset! *env* to) *env*)
 
(defn get-workbench [] #^IWorkbench (PlatformUI/getWorkbench))
(defn get-active-workbench-window [] 
  (aif (get-workbench)
    (.getActiveWorkbenchWindow #^IWorkbench it)))      
(defn get-active-page [] 
  (aif (get-active-workbench-window)
    (.getActivePage #^IWorkbenchWindow it)))  
(defn get-active-editor []
  (aif (get-active-page)
    (.getActiveEditor #^IWorkbenchPage it))) 
(defn get-document [#^LispEditor ed] (.getDocument ed))
 
 

(defstruct editor-env :editor :doc :local-vars :global-vars)

(defnop create-env [&op set partial]
  (acondlet [((:doc partial)
               (item (struct-map editor-env
                       :doc it :editor  (:editor partial)
                       :local-vars nil :global-vars nil)))
             ((get-active-editor)
               (item (struct-map editor-env
                       :editor it :doc (get-document it)
                       :local-vars nil :global-vars nil)))]
    (if set
      (set-env item)
      (atom item))))

(defmacro bind-env [env & body]
 `(binding [*env* (create-env nil ~env)]
    ~@body))
  
(defmacro cdefn [name args & rest]
  (if (= (sym-at name 0) '-)
    (do
      (assert (vector? args))
      `(defn ~name [~'this ~@args] ~@rest))
    `(defn ~name ~args ~@rest)))



(defmacro with-ui [& body]
  `(let [result# (atom nil)]
     (.. PlatformUI (getWorkbench) (getDisplay) 
       (syncExec 
         (fn []
           (reset! result# (do ~@body)))))
     (deref result#)))
     
(defmacro with-aui [& body]
 `(let [result# (atom nil)]
     (.. PlatformUI (getWorkbench) (getDisplay) 
       (asyncExec 
         (fn []
           (reset! result# (do ~@body)))))
     (deref result#)))


(defmacro with-env [& args] `(with-env? true ~@args))

(defmacro with-env? [check args & body]
 (if-not (empty? args)
  (let [keys (map #(symkey 
                     (or (second (mklst %)) %)) 
               args)
        names  (map #(first (mklst %)) args)
        name-key (mapcat #(list % %2) names keys)]
    `(when-let [it# (deref *env*)]
         (let [~(apply hash-map name-key) it#] 
           ~@body)))
   (do ~@body)))

(defn env-var? [sym]      
  (= (first (str (if (seq? sym) (second sym) sym))) \-))

(defn defenv-op [name args extra body]
 `(defnop ~name ~args
    (with-env ~extra ~@body)))

(defmacro defenvfn [name args [& extra] & body]
  (let [[*env-vars others] (split-with env-var? args)]
    (if (empty? *env-vars)
      (defenv-op name args extra body)
      (let [env-vars (map #(if-not (seq? %)
                             (sym-seq % 1)
                             %)
                       *env-vars)   
            env-var-names (map #(if (seq? %)
                                  (first %)
                                  %)
                            env-vars) 
            env-var-types (map #(if (seq? %) (second %) %) env-vars)
            all-args (concat env-var-names others)]
        `(defn ~name
           (~(vec all-args)
             (with-env? true ~extra
               ~@body)) 
           ~@(when-not (= others all-args)
               `((~(vec others)
                   (with-env (~@env-vars) 
                     (~name ~@all-args))))))))))

(defmacro defedfn [name args extra & body]
  `(defenvfn ~name [~@args] [~'doc (~'ed editor) ~@extra]
     ~@body))

(defmacro def-ns [& args]
 `(prog1
    (ns ~@args) 
    (with-ns '~(first args) 
      (def ~'*env* (atom nil)))))
 
(defmacro ubind-env [& rest]
  `(with-ui
     (bind-env ~@rest)))
         