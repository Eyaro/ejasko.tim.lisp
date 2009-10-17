(require 'ecore.env)
(ecore.env/def-ns script.ParenMatcher
  (:gen-class
    :implements [org.eclipse.jface.text.source.ICharacterPairMatcher]
    :state state
    :init init 
    :methods [[setPainter [org.eclipse.jface.text.source.MatchingCharacterPainter] void]])
  (:use ecore.utils.genutils
        [ecore.env 
           :only (defenvfn defedfn with-env get-workbench get-active-workbench-window get-active-page 
                  get-active-editor with-aui with-ui cdefn bind-env ubind-env
                  create-env env get-env set-env)]) 
  (:import
    (org.eclipse.swt SWT)
    (org.eclipse.jface.text IDocument Region)
    (org.eclipse.swt.widgets Display)
    (java.lang Exception)
    (jasko.tim.lisp.util LispUtil)))
  
(defn -init []
  [[] (atom [])])
(cdefn -clear [])
(cdefn -dipose [])

(cdefn -getAnchor [] 1)
(cdefn -setPainter [painter] 
  (reset! (.state this) painter))
 
;;shouldn't be custom, but should be rather in syntax table.... Later, though
(defcustom *default-partition*
  "Default Partition: Includes Open/Close Parentheses"
  IDocument/DEFAULT_CONTENT_TYPE)
  
(defcustom *match*
  "Open/Closed Parentheses finds match"
  SWT/COLOR_DARK_GRAY)
  
(defcustom *nomatch*
  "Open/Closed Parenthes can't find match"
  SWT/COLOR_RED)

(defn get-default-system-color [color]
  (.. Display (getDefault) (getSystemColor color)))

(defn set-painter-color [pnt color]
  (.setColor pnt (get-default-system-color
                     color)))
                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defedfn partition-type [i] [] 
  (.. doc (getPartition i) (getType)))

(defedfn get-char [i &op error] []
  (catch-if (true? error) nil
    (.getChar doc i)))
    
(defnop next-char [i &op (amount 1)] 
  (get-char (inc i)))
  
(defnop previous-char [i &op (amount 1)] 
  (get-char (dec i)))
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
(defmacro mk-region [& args] `(Region. ~@args))

(defn find-matching-paren [doc i]
 (when (= (partition-type i) *default-partition*)
  (let [char-ahead (get-char i)
        char-behind (previous-char i)
        pos (or
              (and (= char-behind \)) (dec i))
              (and (= char-ahead \() i)
              (and (= char-behind \() (dec i))
              (and (= char-ahead \)) i))]
    (when pos
      (let [p (if (= (get-char pos) \))
                  (LispUtil/findOpenParen doc pos)
                  (LispUtil/findCloseParen doc (inc pos)))]
        (if (> p -1)
          {:found true :pos p}
          {:found nil :pos pos}))))))

(cdefn -match [doc i]
 (bind-env {:doc doc} 
   (awhen (find-matching-paren doc i)
    (if (:found it)
        (set-painter-color @(.state this) *match*)
        (set-painter-color @(.state this) *nomatch*))
    (mk-region (:pos it) 1))))




