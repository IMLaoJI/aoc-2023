(ns aoc.file-util
  (:require
    [aoc.string-util :as string-util]
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn read-file
  "Return full file contents from `path`."
  [path]
  (-> path io/resource slurp str/trim-newline))

(defn read-lines
  "Return file contents as collection of rows."
  [path]
  (-> path read-file str/split-lines))

