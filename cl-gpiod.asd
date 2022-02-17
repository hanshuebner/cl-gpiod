;;;; -*- Mode: Lisp -*-

(in-package :cl-user)

(defpackage :cl-gpiod.system
  (:use :cl :asdf))

(in-package :cl-gpiod.system)

(defsystem :cl-gpiod
  :name "cl-gpiod"
  :author "Hans Hübner <hans.huebner@gmail.com>"
  :version "0.0.1"
  :maintainer "Hans Hübner <hans.huebner@gmail.com>>"
  :licence "BSD"
  :description "Common Lisp bindings to the libgpiod library"
  :long-description ""

  :depends-on (:alexandria :cffi)
  :components ((:file "gpiod-ffi")))
