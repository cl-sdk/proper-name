# proper-name

A Common Lisp library for validating and sanitizing **proper names** across multiple languages and scripts.
It ensures that input strings conform to reasonable rules for personal names, while rejecting suspicious Unicode tricks (Zalgo text, invisible characters, control codes) and normalizing common variants (fullwidth, ligatures).

---

## Features

* ✅ Accepts Unicode letters from all scripts (Latin, Greek, Cyrillic, Arabic, Hebrew, etc.).
* ✅ Supports diacritics (combining marks), but **rejects excessive stacking** (prevents Zalgo).
* ✅ Allows common separators: space, NBSP (U+00A0), NARROW NO-BREAK SPACE (U+202F).
* ✅ Allows hyphens and apostrophe variants.
* ✅ Normalizes input using **NFKC** (fullwidth → normal width, compatibility forms normalized).
* ✅ Rejects:

  * Control characters (tabs, newlines, zero-width spaces, etc.)
  * Symbols, emoji, math/currency characters
  * Overlay/enclosing combining abuse and excessive combining runs

---

## Quick Examples

```lisp
(ql:quickload :proper-name)

(proper-name:valid-name-unicode-p "Jean-Luc Picard")
=> T

(proper-name:valid-name-unicode-p "E͠͠͠mil")
=> NIL  ; rejected for excessive combining marks

(proper-name:valid-name-unicode-p "Иван Иванович")
=> T

(proper-name:valid-name-unicode-p "John<TAB>Doe")
=> NIL  ; rejected due to control character
```

---

## API

### `(proper-name:valid-name-unicode-p string &key (max-combining 1))`

Return `T` if `string` is a valid proper name, `NIL` otherwise.

* `max-combining` — maximum number of consecutive combining marks allowed after a base letter. Default = `1`.

---

### Example test snippets (future work)

```lisp
;; Example: Zalgo test
(let ((bad (proper-name:zalgo-generator "Emil" :marks 6)))
  (assert (not (proper-name:valid-name-unicode-p bad))))

;; Example: random whitespace injection
(dotimes (i 100)
  (let ((name (concatenate 'string "Jean" (string (proper-name:random-space-like-char)) "Doe")))
    ;; should be false only if a control char was inserted; otherwise allowed
    (when (proper-name:has-control-chars-p name) (assert (not (proper-name:valid-name-unicode-p name)))))
```

---

## Installation

With Quicklisp:

```lisp
(ql:quickload :proper-name)
```

Add the system to your ASDF config if necessary.

---

## License

This project is released under **The Unlicense**.
