{:user
 {:global-vars {*print-length* 100}
  :dependencies [[alembic "0.3.2"]
                 [org.clojure/tools.nrepl "0.2.12"]]
  :repl-options {:init (set! *print-length* 50)}
  :plugins [[refactor-nrepl "2.0.0-SNAPSHOT"]
            [cider/cider-nrepl "0.11.0-SNAPSHOT"]
            [mvxcvi/puget "1.0.0"]
            ;;[lein-ancient "0.6.8"]
            ]}}
