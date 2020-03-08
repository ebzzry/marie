#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :marie-system
  (:use #:cl #:asdf))

(in-package #:marie-system)

(defsystem :marie
  :name "marie"
  :version "0.0.10"
  :description "My personal miscellaneous utilities"
  :license "CC0"
  :author "Rommel MARTINEZ <ebzzry@ebzzry.io>"
  :depends-on (#:ironclad
               #+sbcl #:sb-posix
               #+sbcl #:sb-sprof)
  :serial t
  :components ((:file "packages")
               (:file "sequences")
               (:file "strings")
               (:file "symbols")
               (:file "misc")
               (:file "collect")
               (:file "files")
               (:file "grids")
               (:file "reader")))
