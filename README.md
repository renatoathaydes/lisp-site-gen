# Lisp Static Site Generator

This is a Common Lisp Project that can be used to generate a static website based on Markdown templates
that can include Lisp expressions, and simple HTML files.

## Packages

`lisp-site-gen` provides the following packages:

* `lisp-site-gen.md-parser` - Simplified Markdown Parser.
* `lisp-site-gen.html-gen` - HTML Generator.

The Parser converts Markdown text files into s-expressions, which can then be fed into the HTML Generator
(or any other library for HTML generation) to create the final website.

## Sub-systems

* `lisp-site-gen/executable` - utility to run `lisp-site-gen` from the terminal.
* `lisp-site-gen/tests` - tests based on the [Rove](https://github.com/fukamachi/rove) testing library.
