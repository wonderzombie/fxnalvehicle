(ns fxnalvehicle.core
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [markdown :as md])
  (:import java.text.SimpleDateFormat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants

(def MULTIMARKDOWN 
  "/usr/local/bin/multimarkdown")

(def MARKDOWN-EXTS
  [".md" ".txt"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Text manipulation and/or parsing.

(defn drop-ext [file]
  (subs file 0
    (.lastIndexOf file ".")))

(defn change-ext [to f]
  (let [noext (drop-ext f)]
    (str noext "." to)))

(defn hasexts? [f exts]
  (reduce #(or %1 %2)
    (map #(.endsWith f %1) exts)))

(defn parse-date [date]
  (.. (SimpleDateFormat. "yyyy-MM-dd")
      (parse date)))

(defn ismarkdown? [f]
  (let [fname (if (map? f) (:filename f) f)]
    (hasexts? fname MARKDOWN-EXTS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These files take and create file metadata.

(defn list-files [dir]
  (filter #(.isFile %) ;; filter out dirs
    (file-seq (io/file dir))))

(defn mk-basic-data [{f :file :as data}]
  (assoc data
    :filename (.getName f)
    :dir (.getParentFile f)))

(defn mk-output-data [{fname :filename dir :dir :as data}]
  (let [outn (change-ext "html" fname)
        outf (io/file dir outn)]
    (assoc data :out outf)))

(defn mk-title-and-date [{fname :filename :as data}]
  (let [noext (drop-ext fname)
        t     (cstr/split noext #"-")
        date  (parse-date (cstr/join "-" (take 3 t)))
        subj  (cstr/join " " (drop 3 t))]
    (assoc data :date date :subject subj)))

(defn mk-all [file]
  (-> {:file file} ;; wrap in basic context.
      mk-basic-data
      mk-output-data
      mk-title-and-date))

(defn gen-metadata [files]
  (->> files
       (map mk-all)
       (filter ismarkdown?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IO related methods. Tread lightly.

(defn doconvert [in out]
  (let [ins  (io/input-stream in)
        outs (io/output-stream out)]
    (do
      (println (format "Converting %s to %s" in out))
      (md/md-to-html in out))))

(defn dogenblog [in out]
  (for [f     (gen-metadata (list-files in))
        outf  (:out f)
        inf   (:file f)]
    (do (println "Input file is: "  inf)
        (println "Output file is: " outf)
        (doto out
          (.delete)
          (.createNewFile))
        (doconvert (:file f) (:out f)))))

(defn -main [& args]
  (let [indir (first args)
        outdir (second args)]
    (dogenblog indir outdir)))
