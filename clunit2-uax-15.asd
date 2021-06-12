;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defsystem "clunit2-uax-15"
  :depends-on ("uax-15" "clunit2" "uiop" "cl-ppcre" "split-sequence" "trivial-benchmark")
  :components
  ((:file "test-package")
   (:file "tests")))
