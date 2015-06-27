(ns node-repl)

(require '[cljs.repl :as repl]
         '[cljs.repl.node :as node])

(repl/repl* (node/repl-env)
      {:output-dir ".cljs_node_repl"
       :cache-analysis true
       :source-map true})
