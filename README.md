marie
=====


Overview
--------

This is a small collection of Common Lisp utilities.


Usage
-----

Clone this repo to `~/common-lisp`:

    $ mkdir -p ~/common-lisp
    $ git clone https://github.com/ebzzry/marie ~/common-lisp/marie

Then load it with [Quicklisp](https://quicklisp.org):

    $ sbcl
    * (ql:quickload :marie)

To print the symbols exported by marie:

    * (marie:external-symbols :marie)
