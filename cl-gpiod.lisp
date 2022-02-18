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
  (or (find-symbol (format nil "+~A-~A+" '#:line-request-direction direction) :gpiod)
      (error "Unknown line direction ~A" direction)))

(defun flag-to-line-request-flag (flag)
  (or (find-symbol (format nil "+~A-~A+" '#:line-request-flag flag) :gpiod)
      (error "Unknown line flag ~A" flag)))

(defun flags-to-line-request-flags (flags)
  `(logior ,@ (mapcar #'flag-to-line-request-flag flags)))

(defun make-line-init (line direction flags)
  `(lambda (chip consumer)
     (let ((handle (gpiod:chip-get-line chip ,line)))
       (cffi:with-foreign-object (config '(:struct gpiod:line-request-config))
         (cffi:with-foreign-slots ((gpiod:consumer gpiod:request-type gpiod:flags) config (:struct gpiod:line-request-config))
           (setf gpiod:consumer consumer
                 gpiod:request-type ,(direction-to-line-request-direction direction)
                 gpiod:flags ,(flags-to-line-request-flags flags)))
         (check (gpiod:line-request handle config 0)))
       handle)))

(defun parse-port (name &key line lines direction flags)
  (if line
      (list
       :init (make-line-init line direction flags)
       :setter `(defun (setf ,name) (value)
                  (gpiod:line-set-value (port-handle ,name) (if value 1 0)))
       :getter `(defun ,name ()
                  (= (gpiod:line-get-value (port-handle ,name)) 1)))))

(defmacro define-gpio (name &key chip-name ports)
  `(progn
     (defvar ,name)
     ))

(defgeneric open-chip (definition-name)
  (:method (definition-name)
    (error "chip ~A not defined" definition-name)))
