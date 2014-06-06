
; required packages
;(require 'asdf)
;(require 'asdf-install)
;(asdf-install:install 'cl-ppcre)

;(load "quicklisp.lisp")
;(quicklisp-quickstart:install :path ".quicklisp/")
;(ql:add-to-init-file)
;(ql:quickload :cl-ppcre)

; packages
(in-package :cl-user)

(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

(in-package :com.gigamonkeys.pathnames)

; 23.1 spam filter core
(defpackage :com.gigamonkeys.spam
  (:use :common-lisp :com.gigamonkeys.pathnames))

(in-package :com.gigamonkeys.spam)

; main function classify
(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

; (defun classification (score)
;   (cond
;     ((<= score *max-ham-score*) 'ham)
;     ((>= score *min-spam-score*) 'spam)
;     (t `unsure)))

; word-feature
(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "この特徴を表す単語")
  (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "この特徴が出現したスパムの数")
  (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "この特徴が出現したハムの数")))

(defvar *feature-database* (make-hash-table :test #'equal))
;(defun clear-database ()
;  (setf *feature-database* (make-hash-table :test #'equal)))

;;
(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))
;;
(defun extract-words (text)
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
    :test #'string=))

; extract-features
;; intern-features
;; extract-words
(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :ham ~d :spam ~d" word ham-count spam-count))))

; (extract-words "foo bar baz")
; (extract-words "foo bar baz for bar")
; (extract-features "foo bar baz foo bar")

; 23.2 train
(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

; (increment-count some-feature 'ham)
; (increment-count some-feature 'spam)

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun clear-database ()
  (setf
    *feature-database* (make-hash-table :test #'equal)
    *total-spams* 0
    *total-hams* 0))

; 23.3 word-based frequency
; (defun spam-probability (feature)
;   (with-slots (spam-count ham-count) feature
;     (/ spam-count (+ spam-count ham-count))))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional
                                  (assumed-probability 1/2)
                                  (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

; 23.4 combining probabilities
(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "Fisher computation by Robinson"
  (inverse-chi-square
;    (* -2 (log (reduce #'* probs)))
    (* -2 (reduce #'+ probs :key #'log))
    (* 2 number-of-probs)))

; 23.5 inverse chi square
(defun inverse-chi-square (value degree-of-freedom)
  (assert (evenp degree-of-freedom))
  (min
    (loop with m = (/ value 2)
       for i below (/ degree-of-freedom 2)
       for prob = (exp (- m)) then (* prob (/ m i))
       summing prob)
   1.0))

; 23.6 let filter learn more
(defun classification (score)
  (values
    (cond
      ((<= score *max-ham-score*) 'ham)
      ((>= score *min-spam-score*) 'spam)
      (t 'unsure))
    score))

; (clear-database)
; (train "Make money fast" 'spam)
; (classify "Make money fast")
; (classify "Want to go to the movies?")

; (train "Do you have any money for the movies?" 'ham)
; (classify "Make money fast")
; (classify "Want to go to the movies?")

; 23.7 testing filter
(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (filename (list-directory dir)) ; defined in Chap 15
    (add-file-to-corpus filename type corpus)))

; (add-directory-to-corpus "mail/spam/" 'spam *corpus*)
; (add-directory-to-corpus "mail/ham/" 'ham *corpus*)

(defun test-classifier  (corpus testing-fraction)
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
         (size (length corpus))
         (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defparameter *max-chars* (* 10 1024))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
        (destructuring-bind (file type) (aref corpus idx)
          (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
        (destructuring-bind (file type) (aref corpus idx)
          (multiple-value-bind (classification score)
              (classify (start-of-file file *max-chars*))
            (list
              :file file
              :type type
              :classification classification
              :score score)))))

; 23.8 utility functions
(defun nshuffle-vector (vector)
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
        (subseq text 0 read)
        text))))

; 23.9 analysis of results
(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
        (ecase classification
          (ham 'correct)
          (spam 'false-positive)
          (unsure 'missed-ham)))
      (spam
        (ecase classification
          (ham 'false-negative)
          (spam 'correct)
          (unsure 'missed-spam))))))

; SPAM> (result-type '(:FILE #p"foo" :type ham :classification ham :score 0))
; CORRECT
; SPAM> (result-type '(:FILE #p"foo" :type spam :classification spam :score 0))
; CORRECT
; SPAM> (result-type '(:FILE #p"foo" :type ham :classification spam :score 0))
; FALSE-POSITIVE
; SPAM> (result-type '(:FILE #p"foo" :type spam :classification ham :score 0))
; FALSE-NEGATIVE
; SPAM> (result-type '(:FILE #p"foo" :type ham :classification unsure :score 0))
; MISSED-HAM
; SPAM> (result-type '(:FILE #p"foo" :type spam :classification unsure :score 0))
; MISSED-SPAM

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

; SPAM> (count-if #'false-positive-p *results*)
; SPAM> (remove-if-not #'false-positive-p *results*)

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive
                 false-negative missed-ham missed-spam))
         (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
          for (label . count) in counts
          do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
                     label count (* 100 (/ count total))))))

; (analyze-results *results*)

(defun explain-classification (file)
  (let* ((text (start-of-file file *max-chars*))
         (features (extract-features text))
         (score (score features))
         (classification (classification score)))
    (show-summary file text classification score)
    (dolist (feature (sorted-interesting features))
      (show-feature feature))))

(defun show-summary (file text classification score)
  (format t "~&~a" file)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f~%" classification score))

(defun show-feature (feature)
  (with-slot (word ham-count spam-count) feature
    (format
     t "~&~2t~a~30thams: ~5d; spams: ~5d;~,10tprob: ~,f~%"
     word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))

