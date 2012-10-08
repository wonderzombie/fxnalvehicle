(ns fxnalvehicle.test.core
  (:use [fxnalvehicle.core])
  (:use [clojure.test]))

(deftest test-drop-ext
  (are [f exp] (= exp (drop-ext f))
    "foo.html"      "foo"
    "foo.bar.html"  "foo.bar"
    ".html"         ""))

(deftest test-change-ext
  (are [to f exp] (= exp (change-ext to f))
    "html"  "foo.md" "foo.html"))

(deftest test-hasexts
  (are [f exts exp] (= exp (hasexts? f exts))
    "foo.txt" ["txt" "md"] true
    "foo.md"  ["txt" "md"] true
    "foo.md"  ["txt"]      false
    "foo"     ["txt" "md"] false))

(deftest test-ismarkdown
  (are [f exp] (= exp (ismarkdown? f))
    "foo.md"    true
    "foo.txt"   true
    "foo.html"  false
    "foo"       false
    "foomd"     false))

(defn date-as-vec [d]
  [(.getYear d) (.getMonthOfYear d) (.getDayOfMonth d)])

(deftest test-parse-date
  (are [d exp] (= exp (date-as-vec (parse-date d)))
    "2012-09-23" [2012 9 23]
    "2012-12-28" [2012 12 28]))

(deftest test-mk-title-and-date
  (are [f exp] (= exp (:subject (mk-title-and-date {:filename f})))
    "2012-09-09-foo-bar-baz.txt" "foo bar baz"
    "2012-09-09-foo.txt"         "foo"
    "2012-12-12-foo_bar_baz.txt" "foo_bar_baz"))