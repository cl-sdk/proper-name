(defpackage #:proper-name
  (:use #:cl)
  (:export
   #:valid-name-unicode-p))

(in-package :proper-name)

(defun valid-name-unicode-p (string &key (max-combining 1))
  "Return T if STRING is a valid proper name by Unicode categories,
 after normalization."
  (let* ((is-valid nil)
         (combining-count 0))
    (loop for ch across (uax-15:normalize string :nfkc)
          for code = (char-code ch)
          do (multiple-value-bind (a cat)
                 (cl-unicode:general-category code)
               (declare (ignorable a))
               (cond
                 ;; Letters (any script, including ligatures)
                 ((member cat '(cl-unicode-names::lu
                                cl-unicode-names::ll
                                cl-unicode-names::lt
                                cl-unicode-names::lm
                                cl-unicode-names::lo))
                  (setf is-valid t combining-count 0))
                 ;; Marks (diacritics)
                 ((member cat '(cl-unicode-names::mn
                                cl-unicode-names::mc
                                cl-unicode-names::me))
                  (incf combining-count)
                  (when (> combining-count max-combining)
                    (return-from valid-name-unicode-p nil)))
                 ;; Spaces
                 ((eq cat 'cl-unicode-names::zs)
                  (setf combining-count 0))
                 ;; Spaces: only allow SPACE, NO-BREAK SPACE, NARROW NO-BREAK SPACE
                 ((member code '(#x0020 #x00A0 #x202F))
                  (setf combining-count 0))
                 ;; Hyphens
                 ((member ch '(#\- #\‐ #\- #\‒ #\– #\— #\―))
                  (setf combining-count 0))
                 ;; Apostrophes
                 ((member ch '(#\' #\’ #\ʻ #\ʼ #\ʽ #\ʾ #\ʿ))
                  (setf combining-count 0))
                 (t (return-from valid-name-unicode-p nil)))))
    is-valid))
