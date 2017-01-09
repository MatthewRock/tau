;;;; tau.lisp

(in-package :cl-user)

(defpackage #:tau
  (:use #:cl #:hash-set)
  (:export :ibm-model-1))

(in-package #:tau)

(declaim (optimize (speed 3) (safety 1)))

(defun ibm-model-1 (english-list foreign-list &key (iterations 1000))
  (declare (type fixnum iterations)
           (type list english-list foreign-list)
           (optimize (speed 3) (safety 1)))
  (let* ((f-keys (list-to-hs foreign-list))
         (so-called-t (make-hash-table :test #'equal :size (hs-count f-keys)))
         (default-t-val (the single-float (/ 1.0 (the integer
                                                      (hs-count f-keys)))))
         (words-combination (* (length english-list)
                               (length foreign-list)))
         (f-keys-len (hs-count f-keys)))
    (loop repeat iterations ; amount of iterations
       for count = (make-hash-table :test #'equal :size words-combination)
       for total = (make-hash-table :test #'equal :size f-keys-len)
       for s-total = (make-hash-table :test #'equal :size f-keys-len)
       do
         (loop for es in english-list
            for fs in foreign-list
            do
              (dolist (e es)
                (setf (gethash e s-total) 0.0)
                (dolist (f fs)
                  (incf (the single-float (gethash e s-total 0.0))
                        (the single-float (gethash (cons e f) so-called-t default-t-val)))))
              (dolist (e es)
                (dolist (f fs)
                  (let ((val (the single-float
                                  (/ (the single-float
                                          (gethash (cons e f) so-called-t default-t-val))
                                     (the single-float
                                          (gethash e s-total))))))
                    (incf (the single-float
                               (gethash (cons e f) count 0.0))
                          (the single-float val))
                    (incf (the single-float (gethash f total 0.0))
                          (the single-float val))))))
         (maphash (lambda (keys _ &aux (f (cdr keys)))
                    (declare (ignore _))
                    (setf (gethash keys so-called-t)
                          (/ (the single-float (gethash keys count 0.0))
                             (the single-float (gethash f total)))))
                  count))
    so-called-t))
