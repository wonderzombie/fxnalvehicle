(ns fxnalvehicle.core
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clj-time.core :as time]
            [clj-time.format :as timef]
            [markdown :as md])
  (:import java.text.SimpleDateFormat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scratch.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants

(def MARKDOWN-EXTS
  [".md" ".mkd" ".txt"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Header-related functions.

(def LINES
  "subject: foo bar baz
date: 2012-10-06
tags: foo bar, baz, quux frotz
---

Hello world.")

(defn- header-end? [line]
  (re-find #"---" line))

(defn take-header [lines]
  (take-while #(not (header-end? %))
    (cstr/split-lines lines)))

(defn header-to-map [lines]
  (let [fields  (map #(cstr/split % #":" 2) lines)
        keys (map (comp keyword first) fields)
        vals (map (comp cstr/trim second) fields)]
    (zipmap keys vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Text manipulation and/or parsing.

(defn drop-ext [fname]
  (subs fname 0
    (.lastIndexOf fname ".")))

(defn change-ext [to f]
  (let [noext (drop-ext f)]
    (str noext "." to)))

(defn hasexts? [f exts]
  (reduce #(or %1 %2)
    (map #(.endsWith f %1) exts)))

(defn parse-date [date]
  (let [fmt (timef/formatters :date)]
    (timef/parse fmt date)))

(defn ismarkdown? [f]
  (let [fname (.getName f)]
    (hasexts? fname MARKDOWN-EXTS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These functions take and create file metadata.

(defn list-files
  ([dir pred]
     (let [files (file-seq (io/file dir))]
        (filter (fn [f]
                  (and (.isFile f) (pred f)))
                files)))
  ([dir]
     (list-files dir identity)))


(def name-bits
  (letfn [(name-split [f]
            (cstr/split f #"-"))]
    (memoize name-split)))

(defn date-part [fname]
  (-> fname
      (name-bits)
      (->> (take 3)
           (cstr/join "-"))
      (parse-date)))

(defn title-part [fname]
  (-> fname
      (drop-ext)
      (name-bits)
      (->> (drop 3)
           (cstr/join " "))))

(defn date-dir [fname]
  (-> fname
      (name-bits)
      (->> (take 3)
           (cstr/join "/"))))

(defn drop-date [srcf]
  (-> srcf
      (name-bits)
      (->> (drop 3)
           (cstr/join "-"))))

(defn output-dir [srcf]
  (let [date-dir (date-dir srcf)
        outn     (change-ext "html" (drop-date srcf))]
    (io/file date-dir outn)))

(defn make-post-data [file]
  (let [srcf  (.getName file)]
    (hash-map 
      :file    file
      :date    (date-part srcf)
      :outfile (output-dir srcf)
      :srcfile srcf
      :title   (title-part srcf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IO related methods. Tread lightly.

(defn doconvert [in out]
  (with-open [rdr  (io/reader in)
              w    (io/writer out)]
    (do
      (println (format "Converting %s to %s" in out))
      (md/md-to-html rdr w))))

(defn initializef [f]
  (doto f
    (.delete)
    (.mkdirs)
    (.createNewFile)))

(defn list-sources [in]
  (let [files (list-files in ismarkdown?)]
    (map make-post-data files)))

(defn dogenblog [sources]
  (doseq [s sources]
    (let [in  (s :file)
          out (s :outfile)]
      (initializef out)
      (doconvert in out))))

;; (defn dogenblog [in out]
;;   (doseq [f (filter #(ismarkdown? (:srcfile %))
;;               (map make-post-data (list-files in))]
;;     (let [inf  (:file f)
;;           outf (io/file out (:outfile f))]
;;       (initializef outf)
;;       (doconvert inf outf))))

(defn in-out-pair [in outdir]
  (let [inf  (:file in)
        outf (io/file outdir (:outfile in))]
    [inf outf]))

(defn -main [& args]
  (let [indir  (first args)
        outdir (second args)]
    (dogenblog indir outdir)))
