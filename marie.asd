#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :marie-system
  (:use #:cl #:asdf))

(in-package #:marie-system)

(defsystem :marie
  :name "marie"
  :version "1.0.0"
  :description "My personal collection of utilities"
  :license "CC0"
  :author "Rommel MARTINEZ <rom@mimix.io>"
  :class :package-inferred-system
  :depends-on ("marie/sequences"
               "marie/strings"
               "marie/symbols"
               "marie/collect"
               "marie/files"
               "marie/grids"
               "marie/reader"
               "marie/etc"))
