(ns csv-utils.core
  (:require
            [clojure-csv.core :as csv]
            [clojure.java.io :as io]))

(defn readCsvFile
  [filename]
  (let [file (slurp filename)]
    (csv/parse-csv file)))

(defn writeCsvFile
  [fileSeq fileName]
  (let [fileData (csv/write-csv fileSeq)]
    (spit fileName fileData)))

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

;; Data sequence must be columnized before being passed in
(defn buildColumnMapRecursive
  [headerSeq dataSeq mapSoFar]
  (if-not (empty? headerSeq)
    (recur (rest headerSeq)
           (rest dataSeq)
           (assoc mapSoFar (first headerSeq) (first dataSeq)))mapSoFar))

;; Builds a map of columns keyed by their headers
(defn buildColumnMapFromFile
  [fileSeq]
  (let [headers (first fileSeq)
        data (rest fileSeq)]
    (buildColumnMapRecursive headers (columnizeData data) {})))

(defn decolumnizeDataMap
  [dataMap columnCount]
  (let [headers (keys dataMap)
        data (vals dataMap)]
    (conj
     (pmap reverse (partition columnCount (apply interleave data)))
     (reverse headers))))

;; ------------- Test Data ----------------
(def fileName "/home/arens/easycsv.csv")
(def file (readCsvFile fileName))

(getColumnCount file)
(getRowCountStripHeaders file)

(buildColumnMapFromFile file)

(columnizeData file)

(reverse (partition 3 (apply interleave (columnizeData file))))

(def testMap (buildColumnMapFromFile file))

(decolumnizeDataMap testMap (count (keys testMap)))

(writeCsvFile (decolumnizeDataMap testMap 3) "/home/arens/mycsv.csv")

(csv/write-csv (decolumnizeDataMap testMap 3))
(get testMap "CustNo")
(getColumnHeaders file)

(def seq1 '(1 2 3))
(def seq2 '(4 5 6))
(def seq3 '(seq1 seq2))

(conj ()  (first seq1) (first seq2))

(keys testMap)

(interleave seq1 seq2)
