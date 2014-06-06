
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

(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *min-spam-score*) 'spam)
    (t `unsure)))

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
    * total-hams* 0))

; 23.3 word-based frequency
(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (/ spam-count (+ spam-count ham-count))))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ smap-frequency ham-frequency)))))

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

