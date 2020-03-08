marie
=====


Overview
--------

This is my personal collection of Common Lisp utilities.


Usage
-----

Clone this repo to `~/common-lisp`:

    $ mkdir -p ~/common-lisp
    $ git clone https://github.com/ebzzry/marie ~/common-lisp/marie

Then load it:

    $ sbcl
    * (asdf:make :marie)

To return the symbols exported by this package:

    * (marie:symbols :marie)


Credits
-------

This system uses list comprehensions written by [Sven-Olof Nyström](http://user.it.uu.se/~svenolof/).
