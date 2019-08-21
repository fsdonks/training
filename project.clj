(defproject training "0.1.0-SNAPSHOT"
  :description "Collection of examples for on-site training."
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [quil       "2.2.6"]]
  :profiles {:reply {:dependencies [[reply "0.4.3"]]
                     :aot [training.main]
                     :main training.main}})
