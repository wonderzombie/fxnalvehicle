(ns fxnalvehicle.test.core
  (:use [fxnalvehicle.core])
  (:use [clojure.test]))

; (deftest replace-me ;; FIXME: write
;   (is false "No tests have been written."))

(deftest test-drop-ext
  (are [f exp] (= exp (drop-ext f))
    "foo.html"      "foo"
    "foo.bar.html"  "foo.bar"
    ".html"         ""))

(deftest test-change-ext
  (are [to f exp] (= exp (change-ext to f))
    "html"  "foo.md" "foo.html"
    ".html" "foo.md" "foo..html"))

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


