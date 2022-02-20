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
                    :event :rising-edge
                    :flags (:bias-pull-up :active-low))
          (button-2 :line 18
                    :event :falling-edge
                    :flags (:bias-pull-up :active-low))))

(defvar *chip*)

(defun open-chip ()
  (when (boundp '*chip*)
    (gpiod:chip-close *chip*))
  (setf *chip* (cl-gpiod:open-chip demo-board "demo")))

(defun test ()
  (format t "Press and release button 2 to start: ") (finish-output)
  (cl-gpiod:wait-for-event 'button-2)
  (terpri) (finish-output)
  (loop
    (format t "Data in: ~A~%" (data-in)) (finish-output)
    (dotimes (i (data-in))
      (setf (data-out) (1+ i))
      (cl-gpiod:wait-for-event-with-timeout 'button-1 1.5))))
