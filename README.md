mof (🦋)
========


Overview
--------

This is a small collection of Common Lisp utilities that I use personally.


Usage
-----

Clone this repo to `~/common-lisp`:

```
$ mkdir -p ~/common-lisp
$ git clone https://github.com/ebzzry/mof ~/common-lisp/mof
```

Then load it:

```
$ sbcl
* (asdf:make :mof)
```

To return the symbols exported by this package:

```
* (mof:symbols :mof)
```


Credits
-------

This system uses list comprehensions written by [Sven-Olof Nyström](http://user.it.uu.se/~svenolof/).
