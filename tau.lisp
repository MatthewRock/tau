;;;; tau.lisp

(in-package :cl-user)

(defpackage #:tau
  (:use #:cl #:hash-set)
  (:export :ibm-model-1))

(in-package #:tau)

(defun ibm-model-1 (english-list foreign-list)
  (let* ((f-keys (list-to-hs foreign-list))
         (so-called-t (make-hash-table :test #'equal :size (hs-count f-keys)))
         (default-t-val (/ 1 (hs-count f-keys))))
    (loop repeat 1000 ; amount of iterations
       for count = (make-hash-table :test #'equal :size (* (length english-list)
                                                           (length foreign-list)))
       for total = (make-hash-table :test #'equal :size (hs-count f-keys))
       for s-total = (make-hash-table :test #'equal :size (hs-count f-keys))
       do
         (loop for es in english-list
            for fs in foreign-list
            do
              (dohashset (e es)
                (setf (gethash e s-total) 0)
                (dohashset (f fs)
                  (incf (gethash e s-total 0)
                        (gethash (cons e f) so-called-t default-t-val))))
              (dohashset (e es)
                (dohashset (f fs)
                  (let ((val (/ (gethash (cons e f) so-called-t default-t-val)
                                (gethash e s-total))))
                    (incf (gethash (cons e f) count 0) val)
                    (incf (gethash f total) val)))))
         (maphash (lambda (keys _ &aux (f (cdr keys)))
                    (declare (ignore _))
                    (setf (gethash keys so-called-t)
                          (/ (gethash keys count 0)
                             (gethash f total))))
                  count))
    so-called-t))
