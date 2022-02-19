;; -*- Lisp -*-

(defpackage :demo-board
  (:use :cl :alexandria))

(in-package :demo-board)

(cl-gpiod:define-gpio demo-board
  :chip-name "gpiochip0"
  :ports ((data-out :lines (23 24 25)
                    :direction :output)
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
  (setf *chip* (cl-gpiod:open-chip 'demo-board "demo")))

(defun test ()
  (setf (data-out) 3)
  (loop until (button-1))
  (setf (data-out) 2)
  (loop until (button-2))
  (setf (data-out) 1))
