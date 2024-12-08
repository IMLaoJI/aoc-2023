(defproject clojure2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [ [postmortem "0.5.2"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/core.match "1.0.0"]
                 [org.flatland/ordered "1.15.11"]
                 [org.clojure/data.int-map "1.2.1"]
                 [org.clojure/clojure "1.11.1"] [hashp "0.2.2"] [philoskim/debux "0.9.1"] [spyscope "0.1.6"] [vvvvalvalval/scope-capture "0.3.3"]]
  :injections [(require 'sc.api) (require 'hashp.core)]
  :repl-options {:init-ns clojure2.core})
