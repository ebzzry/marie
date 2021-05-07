;;;; marie.asd

#-ASDF3.1 (error "ASDF 3.1 or bust!")

(defpackage :marie-system
  (:use #:cl #:asdf))

(in-package #:marie-system)

(defsystem :marie
  :name "marie"
  :version "1.3.10"
  :description "A small collection of Common Lisp utilities"
  :license "CC0"
  :author "Rommel MARTINEZ <rom@mimix.io>"
  :class :package-inferred-system
  :depends-on (#:marie/defs
               #:marie/reader
               #:marie/sequences
               #:marie/strings
               #:marie/symbols
               #:marie/files
               #:marie/etc
               #:marie/driver))
