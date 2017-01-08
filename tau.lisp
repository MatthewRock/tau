;;;; tau.lisp

(in-package :cl-user)

(defpackage #:tau
  (:use #:cl #:hash-set))

(in-package #:tau)

(defun ibm-model-1 (e f)
  (let ((es (list-to-hs e))
        (fs (list-to-hs f))
        (so-called-t (make-hash-table :test #'equal :size (hs-count fs))))
    (loop repeat 1000
       for count = (make-hash-table :test #'equal :size (* (hs-count es)
                                                           (hs-count fs)))
       for total = (make-hash-table :test #'equal :size (hs-count fs))
       for s-total = (make-hash-table :test #'equal)
         do
         (dohashset (e es)
           (hs-insert s-total )
           (dohashset (f fs)
             ())))))
