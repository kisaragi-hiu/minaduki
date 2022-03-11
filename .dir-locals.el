;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (eval . (require 'emacsql))
  (eval . (emacsql-fix-vector-indentation))
  (sentence-end-double-space . nil)
  (nameless-separator . "/")))
