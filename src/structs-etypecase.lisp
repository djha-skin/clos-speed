#+(or)
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.clos-speed/structs-etypecase (:use #:cl)
  (:documentation
    "Speed test using defstruct.")
  (:export
    prepare
    speedtest))

(in-package #:com.djhaskin.clos-speed/structs-etypecase)

(defstruct (op (:conc-name))
  a b)

(defstruct (minus (:include op)))

(defstruct (plus (:include op)))

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
      (let ((a-clause (generate (1- depth)))
            (b-clause (generate (1- depth))))
        (case
            (random 2)
          (0 (make-minus :a a-clause :b b-clause))
          (1 (make-plus :a a-clause :b b-clause))))))

(defun prepare ()
  (loop for i from 1 to 64
        collect (generate 16)))

(defun speed-test (thing)
  (loop for x in thing
        collect (evaluate x)))

(let ((thing (prepare)))
  (time (speed-test thing)))
