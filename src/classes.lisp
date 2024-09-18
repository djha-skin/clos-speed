#+(or)
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.clos-speed/classes (:use #:cl)
  (:documentation
    "Speed test using defclass.")
  (:export
    prepare
    speedtest))

(in-package #:com.djhaskin.clos-speed/classes)

(defgeneric evaluate (thing))
(defgeneric size (thing))

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

(defmethod evaluate ((thing number))
  thing)

(defmethod size ((thing number))
  1)

(defmethod size ((thing op))
  (+ (size (a thing)) (size (b thing))))

(defmethod evaluate ((thing plus))
  (+ (evaluate (a thing)) (evaluate (b thing))))

(defmethod evaluate ((thing minus))
  (- (evaluate (a thing)) (evaluate (b thing))))

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