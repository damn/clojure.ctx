(ns clojure.gdx.dev
  "Starts a dev loop using clojure.tools.namespace.repl/refresh in order to restart the app without
  restarting the JVM.
  Also starts an nrepl server which will keep up even between app crashes and restarts.

  In case of an error, the console prints `WAITING FOR RESTART` and
  the `/restart!` function will restart the app and call `refresh`.

  You can bind this on a key for smooth dev experience, here in VIM:
  ``` vimscript
  nmap <F5> :Eval (do (in-ns 'clojure.gdx.dev)(restart!))
  ```"
  (:require [clojure.java.io :as io]
            [nrepl.server :as nrepl]
            [clojure.tools.namespace.repl :refer [disable-reload!
                                                  refresh]]
            [clj-commons.pretty.repl :as p]))

(disable-reload!) ; keep same connection/nrepl-server up throughout refreshs

(declare ^:no-doc app-ns-sym)

(defn- eval-require-call-main! []
  (eval `(do ; old namespace/var bindings are unloaded with refresh-all so always evaluate them fresh
          (require app-ns-sym)
          (eval (read-string (str "(" (name app-ns-sym) "/-main)"))))))

(def ^:private ^Object obj (Object.))

(defn- wait! []
  (locking obj
    (Thread/sleep 10)
    (println "\n\n>>> WAITING FOR RESTART <<<")
    (.wait obj)))

(def ^:private thrown (atom false))

(defn- handle-throwable! [t]
  (binding [*print-level* 3]
    (p/pretty-pst t 24))
  (reset! thrown t))

(defn restart!
  "Calls refresh on all namespaces with file changes and restarts the application."
  []
  (if @thrown
    (do
     (reset! thrown false)
     (locking obj
       (println "\n\n>>> RESTARTING <<<")
       (.notify obj)))
    (println "\n Application still running! Cannot restart.")))

(declare ^:private refresh-error)

(defn ^:no-doc start-dev-loop! []
  (try (eval-require-call-main!)
       (catch Throwable t
         (handle-throwable! t)))
  (loop []
    (when-not @thrown
      (do
       (.bindRoot #'refresh-error (refresh :after 'clojure.gdx.dev/start-dev-loop!))
       (handle-throwable! refresh-error)))
    (wait!)
    (recur)))

; ( I dont know why nrepl start-server does not have this included ... )
(defn- save-port-file!
  "Writes a file relative to project classpath with port number so other tools
  can infer the nREPL server port.
  Takes nREPL server map and processed CLI options map.
  Returns nil."
  [server]
  ;; Many clients look for this file to infer the port to connect to
  (let [port (:port server)
        port-file (io/file ".nrepl-port")]
    (.deleteOnExit ^java.io.File port-file)
    (spit port-file port)))

(declare ^:private nrepl-server)

(defn -main [& [app-ns]]
  (.bindRoot #'app-ns-sym (symbol app-ns))
  (.bindRoot #'nrepl-server (nrepl/start-server))
  (save-port-file! nrepl-server)
  ;(println "Started nrepl server on port" (:port nrepl-server))
  (start-dev-loop!))
