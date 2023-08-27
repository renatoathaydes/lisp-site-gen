# Contributing

Please submit bug fixes by opening merge requests at https://github.com/renatoathaydes/lisp-site-gen.

For new features and larger changes, create an issue first.

## Adding support for syntax highlithing of new languages

To support a language that is not yet supported, implement the `html-style` protocol
by specializing the following `defgeneric` methods from the `lisp-site-gen.html-code-style:`
package:

* `(html-style-make language-name)`
* `(style-code language text start)`
* `(next-separator language text start)`

`html-style-make` receives a keyword as the `language-name`, so to specialize it for a
language called, for example, `foo`, use:

```lisp
(defmethod html-style-make ((language (eql :FOO)))
    "Whatever is returned by this method is passed as the `language`
    parameter for the other methods."
    :foo)
```

The other methods should specialize based on the `language` parameter with the type of
the object returned by `html-style-make`.

Example where `html-style-make` returned `:foo`:

```lisp
(defmethod style-code ((language (eql :foo)) text start)
    "Return values `t`, the CSS classes as a string,
    and the end-index of the text the style applies to,
    or NIL to not style the text."
    ;; this example always styles the whole text.
    (values t "foo-lang" (length text)))

(defmethod next-separator ((language (eql :foo)) text start)
    "Find the next token separator."
    ;; example returning the position of next whitespace.
    (position #\SPACE text :start start))
```

The `html-style-make` method could return an instance of a class, for example, in order to
keep internal state while going through the code block being styled.

In such case, `style-code` and `next-separator` would specialize on the name of the class:

```lisp
(defclass foo-class () (state))

(defmethod html-style-make ((language (eql :FOO)))
    (make-instance 'foo-class))

(defmethod style-code ((language foo-class) text start) ...)

(defmethod next-separator ((language foo-class) text start) ...)
```
