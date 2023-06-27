# Lisp Static Site Generator

This is a Common Lisp Project that can be used to generate a static website based on Markdown templates
that can include Lisp expressions, and simple HTML files.

## Packages

`lisp-site-gen` provides the following packages:

* `lisp-site-gen.md-parser` - Simplified Markdown Parser.
* `lisp-site-gen.html-code-style` - Code block syntax highlighter.
* `lisp-site-gen.html-gen` - HTML Generator.

The Parser converts Markdown text files into s-expressions, which can then be fed into the HTML Generator
(or any other library for HTML generation) to create the final website.

When the `html-code-style` package is used, it converts plain text from code blocks into syntax-highlighted
HTML s-expressions for consumption by the HTML generator.

## Sub-systems

* `lisp-site-gen/executable` - utility to run `lisp-site-gen` from the terminal.
* `lisp-site-gen/tests` - tests based on the [FiveAM](https://fiveam.common-lisp.dev/docs/) testing library.
