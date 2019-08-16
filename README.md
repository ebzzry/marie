mof 🦋
======


Overview
--------

This is a small collection of miscellaneous Common Lisp utilities.


Usage
-----

Clone this repo to `~/common-lisp`:

    $ mkdir -p ~/common-lisp
    $ git clone https://github.com/a1b10/mof ~/common-lisp/mof

Then load it:

    $ sbcl
    * (asdf:make :mof)

To return the symbols exported by this package:

    * (mof:symbols :mof)


Credits
-------

This system uses list comprehensions written by [Sven-Olof Nyström](http://user.it.uu.se/~svenolof/).
