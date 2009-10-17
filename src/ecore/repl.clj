; repl-server.clj 

(ns ecore.repl
  (:gen-class
     :state state
     :init init
     :methods [[start [] void]])
  (:use clojure.main)
  (:import
     (java.net ServerSocket Socket)
     (java.io InputStreamReader PrintWriter)
     (java.util.concurrent ThreadPoolExecutor TimeUnit LinkedBlockingQueue)
     (java.net BindException)
     (clojure.lang LineNumberingPushbackReader)))


(defn -init [] 
  [[] (atom [])])
  
  
(defn bind-server [port backlog]
  "binds to the given port with the backlog specified"
   (ServerSocket. port backlog))

(defn server-loop [tasklet socket min-threads max-threads]
  "accepts connections and delegates them to the specified task"
  (let [exec (ThreadPoolExecutor. min-threads max-threads 60 TimeUnit/SECONDS
                                  (LinkedBlockingQueue.))]
    (loop []
      (let [accepted-socket (.accept #^ServerSocket socket)]
        (.submit exec #^Callable #(with-open [accepted-socket accepted-socket]
                                             (tasklet accepted-socket))))
      (recur))))

(defn spawn-repl [socket]
  "spawns a repl on a given socket"
  (let [input (new LineNumberingPushbackReader 
                   (new InputStreamReader (.getInputStream socket)))
        output (new PrintWriter (.getOutputStream socket) true)]
    (binding [*in*  input
              *out* output
              *err* output]
      (clojure.main/repl))))

(defn -start [this]
   (future (server-loop spawn-repl #^ServerSocket (bind-server 12345 25) 5 100)))


