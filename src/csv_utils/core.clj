(ns csv-utils.core
  (:require
            [clojure-csv.core :as csv]
            [clojure.java.io :as io]))

(def fileName "/home/arens/samplecsv.csv")
(def file (loadCsvFile fileName))

(defn loadCsvFile
  [filename]
  (let [file (slurp filename)]
    (csv/parse-csv file)))

(defn getColumnHeaders
  [fileSeq]
  (first fileSeq))

(defn getColumnCount
  [fileSeq]
  (let [row (first fileSeq)]
    (count row)))

;; Discards headers
(defn getRowCountStripHeaders
  [fileSeq]
  (let [data (rest fileSeq)]
    (count data)))

;; Assumes no headers on data
(defn getRowCount
  [fileSeq]
  (count fileSeq))

(defn columnizeData
  [fileSeq]
  (partition (getRowCount fileSeq) (apply interleave fileSeq)))

;; Builds a map of columns keyed by their headers
(defn buildColumnMapFromFile
  [fileSeq]
  (let [headers (first fileSeq)
        data (rest fileSeq)]
    (buildColumnMapRecursive headers (columnizeData data) {})))

;; Data sequence must be columnized before being passed in
(defn buildColumnMapRecursive
  [headerSeq dataSeq mapSoFar]
  (if-not (empty? headerSeq)
    (recur (rest headerSeq)
           (rest dataSeq)
           (assoc mapSoFar (first headerSeq) (first dataSeq)))mapSoFar))
     
(buildMapFromFile file)

(def testMap (buildMapFromFile file))

(get testMap "CustNo")
