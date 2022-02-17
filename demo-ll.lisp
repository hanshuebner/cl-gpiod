;; -*- Lisp -*-

(defpackage :demo-ll
  (:use :cl :alexandria))

(in-package :demo-ll)

(defmacro with-open-chip-named ((var chip-name) &body body)
  `(let ((,var (gpiod:chip-open-by-name ,chip-name)))
     (unwind-protect
          (progn ,@body)
       (gpiod:chip-close ,var))))

(defmacro check (api-call)
  (with-gensyms (status)
    `(let ((,status ,api-call))
       (when (minusp ,status)
         (error "~A failed with status ~D" ',api-call ,status))
       ,status)))

(defun demo (&key
               (chip-name "gpiochip0")
               (line-number 24))
  (with-open-chip-named (chip chip-name)
    (format t "~A has ~D lines~%" chip-name (gpiod:chip-num-lines chip))
    (let ((line (gpiod:chip-get-line chip line-number)))
      (check (gpiod:line-request-output line "cl-demo" 0))
      (dotimes (i 5)
        (gpiod:line-set-value line 0)
        (sleep .2)
        (gpiod:line-set-value line 1)
        (sleep .1)))))
