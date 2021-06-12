;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: UAX-15-TESTS; -*-
(in-package :clunit2-uax-15)

(defun parse-hex-string-to-string (str)
  "Takes a string which may be one or more hex numbers e.g. '0044 0307', builds an array of characters, coerces to string and returns the string. Mostly used for testing."
  (let* ((split-str (split-sequence:split-sequence #\Space str :remove-empty-subseqs t))
         (arry (make-array (length split-str))))
    (loop for x in split-str counting x into y do
      (setf (aref arry (- y 1)) (parse-integer x :radix 16)))
    (uax-15:from-unicode-string (coerce arry 'uax-15:unicode-string))))

(defun hs-to-cs (str)
  "Syntactic sugar"
  (parse-hex-string-to-string str))

(defparameter *test-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "t") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'uax-15 nil))))

(defun read-test-data (fname)
  (with-open-file (in (uiop:merge-pathnames* *test-directory* fname))
    (loop for line = (read-line in nil nil)
          while line
          collect (cl-ppcre:split ";" line))))

(defun first-failure (fname fmt)
  "Reports the data line for the first failure for debugging purposes where fname is the test data filename and fmt is e.g. :nfkc"
  (loop for x in (read-test-data fname) counting x into y do
    (when (not (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) fmt)))
      (format t "Failure Line ~a ~a fourth should equal normalized first" y x)
      (return-from first-failure))))

(clunit:defsuite clunit2-uax-15-equal ())

(defparameter *part0* (read-test-data "test-part0.txt"))
(defparameter *part1* (read-test-data "test-part1.txt"))
(defparameter *part2* (read-test-data "test-part2.txt"))
(defparameter *part3* (read-test-data "test-part3.txt"))

(clunit:deftest part0-nfkc (clunit2-uax-15-equal)
  (loop for x in *part0* do
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc))))

(clunit:deftest part1-nfkc (clunit2-uax-15-equal)
  (loop for x in *part1* do
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc))))

(clunit:deftest part2-nfkc (clunit2-uax-15-equal)
  (loop for x in *part2* do
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc))))

(clunit:deftest part3-nfkc (clunit2-uax-15-equal)
  (loop for x in *part3* do
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc))))

(clunit:deftest part0-nfkd (clunit2-uax-15-equal)
  (loop for x in *part0* do
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd))))

(clunit:deftest part1-nfkd (clunit2-uax-15-equal)
  (loop for x in *part1* do
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd))))

(clunit:deftest part2-nfkd (clunit2-uax-15-equal)
  (loop for x in *part2* do
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd))))

(clunit:deftest part3-nfkd (clunit2-uax-15-equal)
  (loop for x in *part3* do
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd))))

(clunit:deftest part0-nfc (clunit2-uax-15-equal)
  (loop for x in *part0* do
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc))))

(clunit:deftest part1-nfc (clunit2-uax-15-equal)
  (loop for x in *part1* do
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc))))

(clunit:deftest part2-nfc (clunit2-uax-15-equal)
  (loop for x in *part2* do
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc))))

(clunit:deftest part3-nfc (clunit2-uax-15-equal)
  (loop for x in *part3* do
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc))
    (clunit:assert-equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc))
    (clunit:assert-equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc))))

(clunit:deftest part0-nfd (clunit2-uax-15-equal)
  (loop for x in *part0* do
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd))))

(clunit:deftest part1-nfd (clunit2-uax-15-equal)
  (loop for x in *part1* do
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd))))

(clunit:deftest part2-nfd (clunit2-uax-15-equal)
  (loop for x in *part2* do
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd))))

(clunit:deftest part3-nfd (clunit2-uax-15-equal)
  (loop for x in *part3* do
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd))
    (clunit:assert-equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd))
    (clunit:assert-equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd))))

(clunit:defsuite clunit2-uax-15-true ())

(clunit:deftest t-part0-nfkc (clunit2-uax-15-true)
  (loop for x in *part0* do
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(clunit:deftest t-part1-nfkc (clunit2-uax-15-true)
  (loop for x in *part1* do
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(clunit:deftest t-part2-nfkc (clunit2-uax-15-true)
  (loop for x in *part2* do
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(clunit:deftest t-part3-nfkc (clunit2-uax-15-true)
  (loop for x in *part3* do
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(clunit:deftest t-part0-nfkd (clunit2-uax-15-true)
  (loop for x in *part0* do
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(clunit:deftest t-part1-nfkd (clunit2-uax-15-true)
  (loop for x in *part1* do
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(clunit:deftest t-part2-nfkd (clunit2-uax-15-true)
  (loop for x in *part2* do
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(clunit:deftest t-part3-nfkd (clunit2-uax-15-true)
  (loop for x in *part3* do
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(clunit:deftest t-part0-nfc (clunit2-uax-15-true)
  (loop for x in *part0* do
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(clunit:deftest t-part1-nfc (clunit2-uax-15-true)
  (loop for x in *part1* do
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(clunit:deftest t-part2-nfc (clunit2-uax-15-true)
  (loop for x in *part2* do
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(clunit:deftest t-part3-nfc (clunit2-uax-15-true)
  (loop for x in *part3* do
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
    (clunit:assert-true (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(clunit:deftest t-part0-nfd (clunit2-uax-15-true)
  (loop for x in *part0* do
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(clunit:deftest t-part1-nfd (clunit2-uax-15-true)
  (loop for x in *part1* do
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(clunit:deftest t-part2-nfd (clunit2-uax-15-true)
  (loop for x in *part2* do
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(clunit:deftest t-part3-nfd (clunit2-uax-15-true)
  (loop for x in *part3* do
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
    (clunit:assert-true (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))


(defun run-assert-equal ()
  (benchmark:with-timing (10)
    (clunit:run-suite 'clunit2-uax-15-equal :report-progress nil)))

(defun run-assert-true ()
  (benchmark:with-timing (10)
    (clunit:run-suite 'clunit2-uax-15-true :report-progress nil)))

#|
Running assert-equal

-                SAMPLES  TOTAL       MINIMUM    MAXIMUM    MEDIAN     AVERAGE    DEVIATION
REAL-TIME        10       513.22675   49.596355  53.53633   50.976345  51.322678  1.218134
RUN-TIME         10       512.9436    49.56916   53.510685  50.942352  51.29436   1.219151
USER-RUN-TIME    10       512.6173    49.539185  53.467484  50.91237   51.261734  1.217517
SYSTEM-RUN-TIME  10       0.326315    0.019992   0.043201   0.029982   0.032632   0.0074
PAGE-FAULTS      10       0           0          0          0          0          0.0
GC-RUN-TIME      10       914.506     72.46      160.375    74.879     91.4506    27.014193
BYTES-CONSED     10       5954820960  594774800  601647184  594790928  595482096  2055083.9
EVAL-CALLS       10       1           0          1          0          0.1        0.3

Running assert-true

  Tested 338760 assertions.
        Passed: 338760/338760 all tests passed
-                SAMPLES  TOTAL       MINIMUM    MAXIMUM    MEDIAN     AVERAGE      DEVIATION
REAL-TIME        10       606.5453    57.6562    63.269527  60.51953   60.654526    1.786767
RUN-TIME         10       606.23285   57.625088  63.23645   60.491238  60.623283    1.786142
USER-RUN-TIME    10       605.9698    57.61177   63.19985   60.461254  60.596977    1.779605
SYSTEM-RUN-TIME  10       0.263067    0.013318   0.043308   0.023321   0.026307     0.009593
PAGE-FAULTS      10       0           0          0          0          0            0.0
GC-RUN-TIME      10       710.419     48.98      113.116    66.376     71.0419      15.874525
BYTES-CONSED     10       3975266656  397506816  397552320  397522016  397526660.0  12273.494
EVAL-CALLS       10       0           0          0          0          0            0.0
NIL
|#
