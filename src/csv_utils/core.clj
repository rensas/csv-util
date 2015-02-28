(ns csv-utils.core
  (:require
            [clojure-csv.core :as csv]
            [clojure.java.io :as io]))

(defn swap
  [v i j] 
  (-> v (assoc i (v j)) (assoc j (v i)))) 

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

(defn swapColumnsRecursive
  [colList dataMap mapSoFar]
  (if-not (empty? colList) 
    (recur (rest colList)
           dataMap
           (assoc mapSoFar
                  (first colList)
                  (get dataMap (first colList))))
  mapSoFar))

(defn swapColumns
  [dataMap col1 col2]
  (let [colList (into [] (keys dataMap))]
    (let [c1 (.indexOf colList col1)
          c2 (.indexOf colList col2)
          swappedColList (swap colList c1 c2)]
      (swapColumnsRecursive (reverse swappedColList) dataMap {}))))

;; ------------- Test Data ----------------
(def fileName "/home/arens/easycsv.csv")
(def file (readCsvFile fileName))
(def testMap (buildColumnMapFromFile file))

(select-keys testMap ["Letters" "Numbers"])

(swapColumns testMap "Letters" "Numbers")
(println testMap)
(def v1 (into [] (keys testMap)))
(def l1 ["A" "B" "C" "D"])

(swap l1 0 2)
(.indexOf v1 "Letters")
(keys testMap)
(get testMap "Letters")

(getColumnCount file)
(getRowCountStripHeaders file)

(swap (buildColumnMapFromFile file) 0 1)

(columnizeData file)

(reverse (partition 3 (apply interleave (columnizeData file))))

(swap testMap "Letters" "Numbers")
(println testMap)

(decolumnizeDataMap testMap (count (keys testMap)))

(writeCsvFile (decolumnizeDataMap testMap 3) "/home/arens/mycsv.csv")

(csv/write-csv (decolumnizeDataMap testMap 3))
(get testMap "CustNo")
(getColumnHeaders file)

(keys testMap)
