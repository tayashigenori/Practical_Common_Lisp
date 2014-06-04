
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

; Chap 23
(defpackage :com.gigamonkeys.spam
  (:use :common-lisp :com.gigamonkeys.pathnames))

(in-package :com.gigamonkeys.spam)

; required packages
;(require 'asdf)
;(require 'asdf-install)
;(asdf-install:install 'cl-ppcre)

;(load "quicklisp.lisp")
;(quicklisp-quickstart:install :path ".quicklisp/")
;(ql:add-to-init-file)
;(ql:quickload :cl-ppcre)

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

; word-features
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
(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

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
