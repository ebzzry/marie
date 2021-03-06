;;;; reader.lisp

(uiop:define-package #:marie/reader
  (:use #:cl
        #:marie/defs))

(in-package #:marie/reader)

(defun bracket-reader (stream char)
  "Use [+ _ 1] as a shorthand for #'(lambda (_) (+ _ 1))
See http://www.bradediger.com/blog/2008/03/stealing_from_arc.html"
  (declare (ignore char))
  `(lambda (,(intern "_") &optional ,(intern "__"))
     (declare (ignorable ,(intern "__")))
     ,(read-delimited-list #\] stream t)))

;; (set-macro-character #\[ #'bracket-reader)
;; (set-macro-character #\] (get-macro-character #\) nil))

(defun brace-reader (stream char)
  "Use {foo 5} as a shorthand for (funcall foo 5)
See http://dorophone.blogspot.com/2008/03/common-lisp-reader-macros-simple.html"
  (declare (ignore char))
  `(funcall ,@(read-delimited-list #\} stream t)))

;; (set-macro-character #\{ #'brace-reader)
;; (set-macro-character #\} (get-macro-character #\) nil))

;;; Return first expression from the last expression evaluated.
(set-dispatch-macro-character
 #\# #\^
 #'(lambda (stream sub-character infix-parameter)
     (declare (ignore stream))
     (when infix-parameter
       (error "#~A does not take an integer infix parameter."
              sub-character))
     `(car +)))

;;; Return last expression from the last expression evaluated.
(set-dispatch-macro-character
 #\# #\$
 #'(lambda (stream sub-character infix-parameter)
     (declare (ignore stream))
     (when infix-parameter
       (error "#~A does not take an integer infix parameter."
              sub-character))
     `(car (last +))))

(defm with-preserved-case ((&optional) &body body)
  "Evaluate BODY while preserving the read case."
  `(let ((*readtable* (copy-readtable nil)))
     (setf (readtable-case *readtable*) :preserve)
     (progn ,@body)))

(def read-from-string* (string)
  "Evaluate STRING with preserved case."
  (with-preserved-case ()
    (read-from-string string)))

(defun lambda-reader (stream char)
  "Define the reader for λ."
  (declare (ignore stream char))
  'LAMBDA)

(set-macro-character #\λ #'lambda-reader)

(defun dollar-reader (stream char)
  (declare (ignore char))
  (list (quote function) (read stream t nil t)))

;;(set-macro-character #\$ #'dollar-reader)
