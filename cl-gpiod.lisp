;; -*- Lisp -*-

(defpackage :cl-gpiod
  (:use :cl :alexandria)
  (:export
   #:define-gpio
   #:open-chip
   #:wait-for-event
   #:wait-for-event-with-timeout))

(in-package :cl-gpiod)

(defvar *port-handles* (make-hash-table))

(defmacro check (api-call)
  (with-gensyms (status)
    `(let ((,status ,api-call))
       (when (minusp ,status)
         (error "~A failed with status ~D" ',api-call ,status))
       ,status)))

(defun port-handle (port-name)
  (or (gethash port-name *port-handles*)
      (error "port ~A not found or not initialized" port-name)))

(defun direction-to-line-request-direction (direction)
  (or (find-symbol (format nil "+~A-~A+" '#:line-request-direction direction) :gpiod)
      (error "Unknown line direction ~A" direction)))

(defun event-to-line-request-event (event)
  (or (find-symbol (format nil "+~A-~A+" '#:line-request-event event) :gpiod)
      (error "Unknown line event ~A" event)))

(defun flag-to-line-request-flag (flag)
  (or (find-symbol (format nil "+~A-~A+" '#:line-request-flag flag) :gpiod)
      (error "Unknown line flag ~A" flag)))

(defun flags-to-line-request-flags (flags)
  `(logior ,@ (mapcar #'flag-to-line-request-flag flags)))

(defun make-init-line-request-config (config consumer direction event flags)
  `(cffi:with-foreign-slots ((gpiod:consumer gpiod:request-type gpiod:flags) ,config (:struct gpiod:line-request-config))
     (setf gpiod:consumer ,consumer
           gpiod:request-type ,(if direction
                                   (direction-to-line-request-direction direction)
                                   (event-to-line-request-event event))
           gpiod:flags ,(flags-to-line-request-flags flags))))

(defun chip-get-line (chip line)
  (let ((handle (gpiod:chip-get-line chip line)))
    (when (cffi:null-pointer-p handle)
      (error "error getting line ~A in chip ~A" line (gpiod:chip-name chip)))
    handle))

(defmacro assert-one-of (a b)
  `(assert (and (or ,a ,b) (not (and ,a ,b))) () ,(format nil "One of ~A or ~A required, but not both" 'a 'b)))

(defun make-line-init (line direction event flags default-val)
  `(let ((handle (chip-get-line chip ,line)))
     (cffi:with-foreign-object (config '(:struct gpiod:line-request-config))
       ,(make-init-line-request-config 'config 'consumer direction event flags)
       (check (gpiod:line-request handle config ,default-val)))
     handle))

(defun make-lines-init (lines direction flags default-val)
  `(let ((bulk (cffi:foreign-alloc '(:struct gpiod:line-bulk)))
         (lines (reverse ',lines)))
     (gpiod:line-bulk-init bulk)
     (dolist (line lines)
       (gpiod:line-bulk-add bulk (chip-get-line chip line)))
     (cffi:with-foreign-object (%default-vals :int (length lines))
       (dotimes (i (length lines))
         (setf (cffi:mem-aref %default-vals :int i) (if (logbitp i ,(or default-val 0)) 1 0)))
       (cffi:with-foreign-object (config '(:struct gpiod:line-request-config))
         ,(make-init-line-request-config 'config 'consumer direction nil flags)
         (check (gpiod:line-request-bulk bulk config %default-vals))))
     bulk))

(defstruct port name init setter getter)

(defun parse-port (name &key
                          line lines
                          direction event
                          flags (default-val 0))
  (assert-one-of line lines)
  (assert-one-of direction event)
  (assert (not (and lines event)) () ":EVENT mode not supported for multi-line ports")
  (cond
    (line (make-port :name name
                     :init (make-line-init line direction event flags default-val)
                     :setter `(defun (setf ,name) (value)
                                (gpiod:line-set-value (port-handle ',name) (if value 1 0)))
                     :getter `(defun ,name ()
                                (= (gpiod:line-get-value (port-handle ',name)) 1))))
    (lines (make-port :name name
                      :init (make-lines-init lines direction flags default-val)
                      :setter `(defun (setf ,name) (value)
                                 (let ((bulk (port-handle ',name)))
                                   (cffi:with-foreign-object (values :int (gpiod:line-bulk-num-lines bulk))
                                     (dotimes (i (gpiod:line-bulk-num-lines bulk))
                                       (setf (cffi:mem-aref values :int i) (ldb (byte 1 i) value)))
                                     (check (gpiod:line-set-value-bulk bulk values)))))
                      :getter `(defun ,name ()
                                 (let ((bulk (port-handle ',name)))
                                   (cffi:with-foreign-object (values :int (gpiod:line-bulk-num-lines bulk))
                                     (check (gpiod:line-get-value-bulk bulk values))
                                     (let ((value 0))
                                       (dotimes (i (gpiod:line-bulk-num-lines bulk))
                                         (setf (ldb (byte 1 i) value) (cffi:mem-aref values :int i)))
                                       value))))))))

(defmacro define-gpio (definition-name &key chip-name ports)
  `(progn
     ,@(let ((ports (mapcar (lambda (port) (apply #'parse-port port)) ports)))
         `((defvar ,definition-name
             (lambda (consumer)
               (let ((chip (gpiod:chip-open-by-name ,chip-name)))
                 (cffi:with-foreign-string (consumer consumer)
                   ,@(mapcar (lambda (port)
                               `(setf (gethash ',(port-name port) *port-handles*)
                                      ,(port-init port)))
                             ports))
                 chip)))
           ,@(mapcar #'port-setter ports)
           ,@(mapcar #'port-getter ports)))))

(defun open-chip (definition consumer)
  (funcall definition consumer))

(defun wait-for-event-internal (tv port)
  (when (plusp (check (gpiod:line-event-wait (gethash port *port-handles*) tv)))
    (cffi:with-foreign-object (event '(:struct gpiod:line-event))
      (check (gpiod:line-event-read (gethash port *port-handles*) event))
      (cffi:with-foreign-slots ((gpiod:event-type) event (:struct gpiod:line-event))
        gpiod:event-type))))

(defun wait-for-event-with-timeout (port timeout)
  (cffi:with-foreign-object (tv '(:struct gpiod:timespec))
    (cffi:with-foreign-slots ((gpiod:tv-sec gpiod:tv-nsec) tv (:struct gpiod:timespec))
      (multiple-value-bind (secs fraction) (truncate timeout)
        (setf gpiod:tv-sec secs
              gpiod:tv-nsec (round (* fraction 1000000000))))
      (wait-for-event-internal tv port))))

(defun wait-for-event (port)
  (wait-for-event-internal (cffi:null-pointer) port))
