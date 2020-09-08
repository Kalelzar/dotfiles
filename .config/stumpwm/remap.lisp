(in-package :stumpwm)


(defmacro remap (win-predicate &rest body)
  `(define-remapped-keys
      '((,win-predicate
         ,@body
         ))))


(remap "[Ff]irefox"
       ("C-y" . "C-v")
       ("C-w" . "C-x")
       ("M-w" . "C-c")
       ("C-k" . "C-w")
       ("C-s" . "C-f")
       ("C-g" . "ESC")
       )
