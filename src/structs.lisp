#+(or)
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.clos-speed/structs (:use #:cl)
  (:documentation
    "Speed test using defstruct.")
  (:export
    prime
    prepare
    speed-test))

(in-package #:com.djhaskin.clos-speed/structs)

(defgeneric evaluate (thing))
(defgeneric size (thing))

(defstruct (op (:conc-name))
  a b)

(defstruct (minus (:include op)))

(defstruct (plus (:include op)))

(defmethod evaluate ((thing number))
  thing)

(defmethod size ((thing number))
  1)

(defmethod evaluate ((thing plus))
  (+ (evaluate (a thing)) (evaluate (b thing))))

(defmethod evaluate ((thing minus))
  (- (evaluate (a thing)) (evaluate (b thing))))

(defmethod size ((thing op))
  (+ (size (a thing)) (size (b thing))))

(defun generate (depth)
  (if (zerop depth)
      (random 10000)
      (let ((a-clause (generate (1- depth)))
            (b-clause (generate (1- depth))))
        (case
            (random 2)
          (0 (make-minus :a a-clause :b b-clause))
          (1 (make-plus :a a-clause :b b-clause))))))

(defun prime ()
  (evaluate (generate 3)))

(defun prepare ()
  (loop for i from 1 to 64
        collect (generate 16)))

(defun speed-test (thing)
  (loop for x in thing
        collect (evaluate x)))
