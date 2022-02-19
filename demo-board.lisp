;; -*- Lisp -*-

(defpackage :demo-board
  (:use :cl :alexandria))

(in-package :demo-board)

(cl-gpiod:define-gpio demo-board
  :chip-name "gpiochip0"
  :ports ((data-out :lines (25 24 23 22)
                    :direction :output)
          (data-in :lines (21 20 19 16)
                   :direction :input
                   :flags (:bias-pull-up :active-low))
          (button-1 :line 17
                    :direction :input
                    :flags (:bias-pull-up :active-low))
          (button-2 :line 18
                    :direction :input
                    :flags (:bias-pull-up :active-low))))

(defvar *chip*)

(defun open-chip ()
  (when (boundp '*chip*)
    (gpiod:chip-close *chip*))
  (setf *chip* (cl-gpiod:open-chip demo-board "demo")))

(defun wait-for-key ()
  (loop until (button-1))
  (sleep .5)
  (loop until (not (button-1))))

(defun test ()
  (loop
    (format t "Data in: ~A~%" (data-in)) (finish-output)
    (dotimes (i (data-in))
      (setf (data-out) (1+ i))
      (wait-for-key))))
