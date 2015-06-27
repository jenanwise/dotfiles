(ns prep-clj)

(defn prep-clj!
  []
  (doseq [sym '[;;cljs.repl.rhino
                ;;cljs.repl.node
                cljs.closure
                cljs.core]]
    (println "Pre-compiling" (name sym))
    (compile sym)))

(prep-clj!)
