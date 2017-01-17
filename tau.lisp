;;;; tau.lisp

(in-package :cl-user)

(defpackage #:tau
  (:use #:cl #:hash-set #:command-line-arguments)
  (:export :main))

(in-package #:tau)

(declaim (optimize (speed 3) (safety 1)))

(defun ibm-model-1 (f-list english-list &key (iterations 1000))
  (declare (type fixnum iterations)
           (type list f-list english-list)
           (optimize (speed 3) (safety 1)))
  (setf lparallel:*kernel* (lparallel:make-kernel 4))
  (loop for i from 0 below 7 do (setf (sb-ext:generation-minimum-age-before-gc i) 0.4d0))
  (let* ((foreign-list (lparallel:pmapcar (lambda (x) (cons "NULL" x)) f-list))
         (f-keys (let ((temp (make-instance 'hash-set)))
                   (lparallel:pmapc (lambda (x)
                           (lparallel:pmapc (lambda (y)
                                         (setf temp (hs-insert temp y))
                                         nil)
                                   x))
                        foreign-list)
                   temp))
         (words-combination (* (length english-list)
                               (length foreign-list)))
         (so-called-t (make-hash-table :test #'equal :size words-combination))
         (default-t-val (the single-float (/ 1.0 (the integer
                                                      (hs-count f-keys)))))
         (f-keys-len (hs-count f-keys)))
    (loop repeat iterations
       for count = (make-hash-table :test #'equal :size words-combination)
       for total = (make-hash-table :test #'equal :size f-keys-len)
       do
         (loop for es in english-list
            for fs in foreign-list
            for s-total = (make-hash-table :test #'equal :size f-keys-len)
            do
              (dolist (e es)
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
    (values so-called-t
            0.0)))

;; (reduce #'- (mapcar (lambda (f e)
;;                       (reduce #'+ (mapcar (lambda (f-w e-w)
;;                                             (log (gethash (cons e-w f-w) so-called-t) 2))
;;                                           f e) :initial-value 0.0))
;;                     f-list english-list) :initial-value 0.0)

(defun split-line (line)
  (declare (type string line))
  (cl-ppcre:split " " (format nil "~(~A~)" line)))

(defun train (english-file foreign-file &optional (iterations 1000))
  (ibm-model-1 (make-corpora foreign-file) (make-corpora english-file) :iterations iterations))

(defun make-corpora (filepath)
  (with-open-file (in filepath)
    (loop for line = (read-line in nil 'eof nil)
       while (not (eq 'eof line)) collect (split-line line))))

(defparameter +command-line-spec+
  '(((#\e "english") :type string :documentation "English corpora file.")
    ((#\f "foreign") :type string :documentation "Foreign corpora file.")
    ((#\i "iterations") :type integer :optional t :documentation "Amount of iterations, 1000 by default.")
    ((#\h #\? "help") :type boolean :optional t :documentation "Show help.")))

(defun train-and-print (args english foreign iterations &key help)
  (when help (progn (show-option-help +command-line-spec+ :sort-names t) (uiop:quit)))
  (multiple-value-bind (praw perplexity) (train english foreign (or (and iterations
                                                                         (parse-integer (car iterations)))
                                                                    1000))
    (maphash (lambda (k v)
               (when (> v 0.001)
                 (format t "~A~T~A~T~,4F~%" (cdr k) (car k) v)))
             praw)
    (format t "~%~%Perplexity: ~F~%" perplexity)))

(defun main (args)
  (handle-command-line
   +command-line-spec+
   'train-and-print
   :command-line args
   :name "Ibm model 1 in Lisp"
   :positional-arity 3
   :rest-arity t))
