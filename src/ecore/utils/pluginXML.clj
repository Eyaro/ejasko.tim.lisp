(ns ecore.utils.pluginXML)

(ns ecore.xml (:use clojure.contrib.prxml clojure.contrib.accumulators))
;;;UTILS
(defn hash-map-list [lst] (apply hash-map lst))
(defn tack [lst item]
  (concat lst (list item)))
;;;
(defmacro emenu [menu-label [id  menu-type] & items]
  `(extension "org.eclipse.ui.menus"
     [:menuContribution {:locationURI ~(parse-menu-type menu-type)}
      [:menu {:id ~id :label ~menu-label} ~@items]]))
(defmacro ecommand [& args]
    `(extension "org.eclipse.ui.commands" [:command ~(hash-map-list args)]))
(defmacro command [& args]
    `[:command ~(hash-map-list args)])

(defn provide-defaults [args defaults overrides]
  (merge
    (merge defaults args)
    overrides))

(defmacro with-defaults [[args defaults override] & body]
  `(let [~args (hash-map-list ~args)
         ~args (provide-defaults ~args ~(hash-map-list defaults) ~(hash-map-list override))]
     ~@body))


(defmacro separator [& args]
  (with-defaults (args (:name (genstr "separator")))
  `[:separator ~args]))

(defmacro with-xml [by & args]
  `(binding [*prxml-indent* ~by]
     (prxml ~@args)))

(defn parse-context [context]
  (condp = context
    :window "org.eclipse.ui.contexts.window"
    context))

(defn parse-category [cat]
  (condp = cat
    :window "org.eclipse.ui.category.window"
    cat))

(defmacro extension [ty & args]
  `[:extension {:point ~ty} ~@args])

(defmacro key-bind [& args]
  `(extension "org.eclipse.ui.bindings"
     [:key ~(hash-map-list args)]))

(defn parse-scheme [scheme]
  (condp = scheme
    :default "org.eclipse.ui.defaultAcceleratorConfiguration"
    :emacs "org.eclipse.ui.emacsAcceleratorConfiguration"
    scheme))

(defmacro easy-key-bind [key-sequence key-context command-id & key-scheme] (print key-scheme)
  `(key-bind
     :commandId ~command-id
     :contextId ~(parse-context key-context)
     :schemeId ~(parse-scheme (or (first key-scheme) :default))
     :sequence ~key-sequence))

(defmacro easy-key-binds [[key-context command-id] & the-keys]
  (assert (string? command-id))
  `(list ~@(map (fn [item]
                  (let [[key-sequence key-scheme] (if-not (vector? item) (vec (list item)) item)]
                    `(easy-key-bind ~key-sequence ~key-context ~command-id ~key-scheme))) the-keys)))

(defn parse-menu-type [menu-type]
  (condp = menu-type
    :main "menu:org.eclipse.ui.main.menu"
    menu-type))

(defmacro easy-menu [label [id menu-type] & args]
  `(emenu ~label [~id ~menu-type]
     ~@(map (fn [item] (if (and (coll? item) (string? (first item)))
                         `(command :commandId ~(second item) :label ~(first item) ~@(nnext item))
                         item))
         args)))



(comment "EXAMPLE"
  ;;create xml
  (with-xml 2
    ;;creation of key binding
    (ecommand :name "Key Handle" :id "rcp.KeyHandle" :defaultHandler "rcp.KeyHandle")
    (easy-key-bind "Ctrl+R" :window "rcp.KeyHandle")
    ;;creation of menu and menu item
    (ecommand :name "Exit" :id "rcp.exit" :defaultHandler "rcp.ExitHandle")
    (easy-menu "File"  ["fileMenu" :main]
      ("Exit" "rcp.exit" :tooltip "Exit Application")
      (separator :visible true)))
  ;;create classes
  (ns rcp.test-handlers
    (:import (org.eclipse.ui.handlers HandlerUtil))
    (:use rcp.handlers))

  (new-handler-class rcp.ExitHandle event
    (.close (HandlerUtil/getActiveWorkbenchWindow event)))
  (new-handler-class rcp.KeyHandle event
    (println event))
  )























(comment "I might very well get rid of all of this. Deprecated?"
 (defmacro ekey [& args]
 `[:key ~(apply hash-map args)])

(defmacro separator [nm] `[:separator {:name ~nm}])
(defmacro separators [& nms] `(list ~@(map (fn [item] `(separator ~item)) nms)))

(defmacro editor-menu [the-id target-id [id label & seps]]
  `(extension "org.eclipse.ui.editorActions"
     [:editorContribution {:targetId ~target-id :id ~the-id}
      [:menu {:id ~id :label ~label}
       (separators ~@seps)]]))



(defmacro editor-menu-item [target-id id [label action-class path] & args]
 (let [new-args (provide-defaults args {:id action-class :definitionId action-class})]
   `(extension "org.eclipse.ui.editorActions"
       [:editorContribution
        {:targetID ~target-id :id ~id}
        [:action ~(merge {:class action-class :label label :path path}
                    new-args)]])))




;;;;;
(defn- equiv [item m]
  (if-not (string? (first item))
    (let [item (find m (first item))]
      (assert (not (nil? item)))
      (second item))
    (first item)))



(defmacro editor-menu-items [id editor-type separator-name & items]
  `(list ~@(map (fn [the-item]
                  (let [p (list (first the-item) (second the-item))
                        others (nnext the-item)]
                    `(editor-menu-item ~id ~editor-type  [~@p ~separator-name] ~@others)))
     items)))

(defmacro editor-menu-and-items [id editor-type args & items]
   (let [names (nnext args)
         m (hash-map-list (interleave (iterate inc 1) names))
         new-items (map #(list* (equiv % m) (next %)) items)]  
  `(list
     (editor-menu ~id ~editor-type ~args)
     ~@(map (fn [item]
             `(editor-menu-items ~editor-type ~id  ~(first item) ~@(next item)))
        new-items))))

  )
;;;
;;;;;;;;;;;;;;








