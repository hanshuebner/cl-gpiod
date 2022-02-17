;; -*- Lisp -*-

(defpackage :config-example
  (:use :cl :alexandria))

(in-package :config-example)

(define-gpio-bits "gpiochip0"
    ((data-out :lines (4 17 27 22 10 9 11 5)
               :direction :output)
     (data-in :lines (6 13 19 26 21 20 16 12)
              :direction :input)
     (in-write :line 23
               :direction :input
               :flags (:pull-down :active-low))
     (in-read :line 18
              :direction :input
              :flags (:pull-down :active-low))
     (out-read :line 25
               :direction :output)
     (out-write :line 24
                :direction :output)))
