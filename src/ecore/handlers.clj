;; handlers
;;;add polymorphism to all
(comment 
(ns ecore.handlers
  (:use 
    ecore.utils.genutils clojure.contrib.condition
    [ecore.env :only (get-active-page with-ui get-active-editor)])
  (:import 
    (org.eclipse.ui.commands ICommandService)
    (org.eclipse.ui.handlers IHandlerService)
    (org.eclipse.core.commands ExecutionEvent AbstractHandler Command Category)
    (org.eclipse.ui.handlers HandlerUtil)
    (org.eclipse.swt.widgets Display)
    ))

(defn find-view [id]
  (aif (get-active-page)
    (.findView  it id)))

(defn get-active-part []
  (aif (get-active-page)
    (.getActivePart it)))
;;;;;;class Polymorphism 
;;;;;;;;
(defn dispatch-class-or-else [& args] 
  (if-not (first args)
    :else
   (class (first args))))
   
(defmulti get-site
  "Gets the site.
Types:
(1) String id for view in current page
 (3) Object
(4) not given, uses active part
Return: IPartSite or nil" dispatch-class-or-else)
(defmethod get-site String [id]
  (aif (find-view id) (.getSite it)))
(defmethod get-site Object [part]
  (.getSite part))
(defmethod get-site :else []
  (aif (get-active-part) (.getSite it)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti get-service
  "Gets service:
(1) Any object (doesn't test if has service function!)
(2) default - gets current site and then gets service"
  (fn [& args] (class (first args))))
(defmethod get-service org.eclipse.ui.internal.PartSite  [site type]
  (.getService site type))
(defmethod get-service :default [obj type]
  (aif (get-site obj) (get-service it type)))
(defmethod get-service java.lang.Class [type]
  (aif (get-site) (get-service it type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defnop get-handler-service [& obj]
  (if-let [ob (or obj (get-site))]
    (get-service ob IHandlerService)))
(defnop get-command-service [& obj]
  (if-let [ob (or obj (get-site))]
   (get-service ob ICommandService)))
;;
(defmulti get-shell
  "Types: ExecutionEvent, none"
  dispatch-class-or-else)
(defmethod get-shell ExecutionEvent [event]
  (HandlerUtil/getActiveShell event))
(defmethod get-shell :else []
  (.. Display (getDefault) (getActiveShell)))

(defmacro uget-shell [& args] `(with-ui (get-shell ~@args)))
;;;;;;;;
;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;
;;;;;;;;TODODOODODODODOD
(defmacro new-abstract-handler [var & body]
  `(proxy [AbstractHandler] []
     (execute [~var]
       ~@body)))

(defn define [obj args]
  (apply .define obj args))
  
(defn get-category [service str]
  #^Category (.getCategory service str))
(defn create-category [service str & args]
  (define (get-category service str) args))
(defn create-category-if [service str & args]
   (aif (get-category service str)
      it
      (define it args))) 
      
(defn get-command [service str]
  #^Command (.getCommand service str))
(defn create-command [service str & args]
  (define (get-command service str) args))
(defn create-command-if [service str & args]
   (aif (get-command service str)
     it
     (define it args)))
  ;;;;;   
(get-category
 (get-command-service)
 "z.ex.view.keybindings.category")
(get-command "testit")
;;;;;;;
(defn activate-handler [service id execution-event]
  (.activateHandler service id execution-event))
(active-handler
  (get-handler-service) id
  (new-abstract-handler event
    nil))

(activate-handler
  (get-handler-service)
  id
  (new-abstract-handler event
    (new-message
      (get-shell this) "HI" *warning*)
    nil))

)

