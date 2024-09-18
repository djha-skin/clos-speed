#+(or)
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.clos-speed/classes-etypecase (:use #:cl)
  (:documentation
    "Speed test using defclass.")
  (:export
    prepare
    speedtest))

(in-package #:com.djhaskin.clos-speed/classes-etypecase)
(defclass op ()
  ((a
     :initarg :a
     :accessor a)
   (b
     :initarg :b
     :accessor b)))

(defclass minus (op)
  ())

(defclass plus (op)
  ())

(defun evaluate (thing)
  (etypecase thing
    (number thing)
    (minus (- (evaluate (a thing))
              (evaluate (b thing))))
    (plus (+ (evaluate (a thing))
             (evaluate (b thing))))))

(defun size (thing)
  (etypecase thing
    (number thing)
    (minus (1+ (size (a thing))))
    (plus (1+ (size (a thing))))))

(defun generate (depth)
  (if (zerop depth)
      (random 10000)
      (make-instance
        (case
            (random 2)
          (0
           'minus)
          (1
           'plus))
        :a
        (generate (1- depth))
        :b
        (generate (1- depth)))))

(defun prepare ()
  (loop for i from 1 to 64
        collect (generate 16)))

(defun speed-test (thing)
  (loop for x in thing
        collect (evaluate x)))

(time (let ((thing (prepare)))
  (speed-test thing)))