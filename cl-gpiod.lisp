;; -*- Lisp -*-

(defpackage :cl-gpiod
  (:use :cl :alexandria)
  (:export
   #:define-gpio
   #:open-chip))

(in-package :cl-gpiod)

(defvar *port-handles* (make-hash-table))

(defmacro check (api-call)
  (with-gensyms (status)
    `(let ((,status ,api-call))
       (when (minusp ,status)
         (error "~A failed with status ~D" ',api-call ,status))
       ,status)))

(defun port-handle (port-name)
  (gethash port-name *port-handles*))

(defun direction-to-line-request-direction (direction)
  (ecase direction
    (:input 'gpiod:+line-request-direction-input+)
    (:output 'gpiod:+line-request-direction-output+)))

(defun flags-to-line-request-flags (flags)
  `(logior ,@(loop for flag in flags
                   collect (ecase flag
                             (:pull-up 'gpiod:+line-request-flag-bias-pull-up+)
                             (:pull-down 'gpiod:+line-request-flag-bias-pull-down+)))))

(defun make-line-init (line direction flags)
  `(lambda (chip consumer-name)
     (let ((handle (check (gpiod:chip-get-line chip ,line))))
       (cffi:with-foreign-object (config 'gpiod:line-request-config)
         (cffi:with-foreign-slots ((consumer request-type flags) config 'gpiod:line-request-config)
           (setf consumer consumer-name
                 request-type ,(direction-to-line-request-direction direction)
                 flags ,(flags-to-line-request-flags flags)))
         (check (gpiod:line-request handle config 0)))
       handle)))

(defun parse-port (name &key line lines direction flags)
  (if line
      (list
       :init (make-line-init line direction flags)
       :setter `(defun (setf ,name) (value)
                  (gpiod:line-set-value (port-handle ,name) (if value 1 0)))
       :getter `(defun ,name ()
                  (gpiod:line-get-value (port-handle ,name))))))

(defmacro define-gpio (name &key chip-name ports)
  `(progn
     (defvar ,name)
     ))

(defgeneric open-chip (definition-name)
  (:method (definition-name)
    (error "chip ~A not defined" definition-name)))
