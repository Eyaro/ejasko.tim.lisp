(ns ecore.utils.handlers
 ;(:import (org.eclipse.ui.handlers HandlerUtil))
 )

(defmacro new-handler-class
  [classname-sym event & body]
  (let [classname        (name classname-sym)
        last-dot         (.lastIndexOf classname ".")
        simple-classname (if (not= last-dot -1)
                           (.substring classname (inc last-dot))
                           classname)
        prefix (str (gensym simple-classname))
        prefixed         #(symbol (str prefix %))
        hinted-this      (with-meta 'this {:tag classname-sym})]
    `(do 
       (gen-class
         :name    ~classname-sym
         :extends org.eclipse.core.commands.AbstractHandler
         :prefix  ~prefix
         :init    ~'init
         :main    false)
       (defn ~(prefixed "init")
         []
         [[] []])
       (defn ~(prefixed "execute")
         [~hinted-this ~event]
         ~@body))))