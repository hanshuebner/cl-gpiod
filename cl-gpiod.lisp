;; -*- Lisp -*-

(defpackage :cl-gpiod
  (:use :cl :alexandria)
  (:export
   #:define-gpio
   #:open-chip))

(in-package :cl-gpiod)

(defmacro define-gpio (name (&key chip-name lines))
  `(progn))

(defun open-chip (definition))
