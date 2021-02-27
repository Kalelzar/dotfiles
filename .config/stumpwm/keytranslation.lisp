(in-package :stumpwm)

;;;; NEW

(defun keysym (name)
  (stumpwm::keysym-name->keysym name))

(defstruct translation-table
  (layout "bg" :type string)
  (a->b nil :type hash-table)
  (b->a nil :type hash-table))

(defvar *key-translations* nil
  "List of key translations between different keyboard layouts.
Allows for seamless use of keybindings in all layouts.")

;; (setq *key-translations* nil)

(defun make-key-translation (layout map)
  (let ((a->b (make-hash-table))
        (b->a (make-hash-table)))
      (dolist (key map)
        (let ((in-layout-1 (car key))
              (in-layout-2 (cdr key)))
          (setf (gethash in-layout-1 a->b) in-layout-2
                (gethash in-layout-2 b->a) in-layout-1)))
      (setf *key-translations* (append *key-translations*
                                       (list (make-translation-table
                                              :layout layout
                                              :a->b a->b
                                              :b->a b->a))))))

(defun eval-key-translation (to)
  (let* ((cmd-template (concat kal/config-directory
                              "utils/equivmodmap \"~D\" \"~D\""))
         (cmd (format nil
                      cmd-template
                      *default-layout*
                      to)))
                                        ;(eval (read-from-string (run-shell-command cmd t)))
    cmd))


;(dolist (layout-pair *available-keyboard-layouts*)
;  (let ((layout (concat (car layout-pair) " " (cdr layout-pair))))
;    (unless (eq layout *default-layout*)
;    (eval-key-translation layout))))







;;;; OLD

(defvar *cyrillic->latin* (make-hash-table)
  "A conversion table between keysyms from cyrillic to latin.")

(defvar *latin->cyrillic* (make-hash-table)
  "A conversion table between keysyms from latin to cyrillic.")

(defun latin<->cyrillic (latin cyrillic)
  "Define a mapping from a LATIN keysym to a CYRILLIC one"
  (setf (gethash latin *latin->cyrillic*) cyrillic
        (gethash cyrillic *cyrillic->latin*) latin))

(defun latin->cyrillic (latin)
  "Return the cyrillic character equivalent to the LATIN character.
Equivalent as in position on the keyboard."
  (multiple-value-bind (value present-p)
      (gethash latin *latin->cyrillic*)
    (declare (ignore present-p))
    value))

(defun cyrillic->latin (latin)
  "Return the latin character equivalent to the CYRILLIC character.
Equivalent as in position on the keyboard."
  (multiple-value-bind (value present-p)
      (gethash latin *cyrillic->latin*)
    (declare (ignore present-p))
    value))

(defun key-alternatives (key)
  (let* ((keysym (key-keysym key))
        (updated-key (make-key :keysym (latin->cyrillic keysym)
                               :shift (key-shift key)
                               :control (key-control key)
                               :meta (key-meta key)
                               :alt (key-alt key)
                               :hyper (key-hyper key)
                               :super (key-super key))))
    (if (null (key-keysym updated-key))
        (list key)
        (list key updated-key))))

(defun kbd-with-alternatives (keybinding)
  (let* ((key (kbd keybinding)))
    (key-alternatives key)))

(defun keysym (name)
  (stumpwm::keysym-name->keysym name))

(latin<->cyrillic (keysym "A") (keysym "Cyrillic_A"))   ; A -> А
(latin<->cyrillic (keysym "B") (keysym "Cyrillic_BE"))  ; B -> Б
(latin<->cyrillic (keysym "C") (keysym "Cyrillic_TSE")) ; C -> Ц
(latin<->cyrillic (keysym "D") (keysym "Cyrillic_DE"))  ; D -> Д
(latin<->cyrillic (keysym "E") (keysym "Cyrillic_IE"))  ; E -> Е
(latin<->cyrillic (keysym "F") (keysym "Cyrillic_EF"))  ; F -> Ф
(latin<->cyrillic (keysym "G") (keysym "Cyrillic_GHE"))  ; G -> Г
(latin<->cyrillic (keysym "H") (keysym "Cyrillic_HA"))  ; H -> Х
(latin<->cyrillic (keysym "I") (keysym "Cyrillic_I"))  ; I -> И
(latin<->cyrillic (keysym "J") (keysym "Cyrillic_SHORTI"))  ; J -> Й
(latin<->cyrillic (keysym "K") (keysym "Cyrillic_KA")); K -> К
(latin<->cyrillic (keysym "L") (keysym "Cyrillic_EL")); L -> Л
(latin<->cyrillic (keysym "M") (keysym "Cyrillic_EM")); M -> М
(latin<->cyrillic (keysym "N") (keysym "Cyrillic_EM")); N -> Н
(latin<->cyrillic (keysym "O") (keysym "Cyrillic_O")); O -> О
(latin<->cyrillic (keysym "P") (keysym "Cyrillic_PE")); P -> П
(latin<->cyrillic (keysym "Q") (keysym "Cyrillic_YA")); Q -> Я
(latin<->cyrillic (keysym "R") (keysym "Cyrillic_ER")); R -> Р
(latin<->cyrillic (keysym "S") (keysym "Cyrillic_ES")); S -> С
(latin<->cyrillic (keysym "T") (keysym "Cyrillic_TE")); T -> Т
(latin<->cyrillic (keysym "U") (keysym "Cyrillic_U")); U -> У
(latin<->cyrillic (keysym "V") (keysym "Cyrillic_ZHE")); V -> Ж
(latin<->cyrillic (keysym "W") (keysym "Cyrillic_VE")); W -> В
(latin<->cyrillic (keysym "X") (keysym "Cyrillic_SOFTSIGN")); X ->
(latin<->cyrillic (keysym "Y") (keysym "Cyrillic_HARDSIGN")); Y -> Ъ
(latin<->cyrillic (keysym "Z") (keysym "Cyrillic_ZE")); Z -> З
(latin<->cyrillic (keysym "asciitilde") (keysym "Cyrillic_CHE"));~ -> Ч
(latin<->cyrillic (keysym "braceleft") (keysym "Cyrillic_SHA"));{ -> Ш
(latin<->cyrillic (keysym "braceright") (keysym "Cyrillic_SHCHA"));} -> Щ
(latin<->cyrillic (keysym "bar") (keysym "Cyrillic_YU"));| -> Ю

(latin<->cyrillic (keysym "a") (keysym "Cyrillic_a"))   ; a -> а
(latin<->cyrillic (keysym "b") (keysym "Cyrillic_be"))  ; b -> б
(latin<->cyrillic (keysym "c") (keysym "Cyrillic_tse")) ; c -> ц
(latin<->cyrillic (keysym "d") (keysym "Cyrillic_de"))  ; d -> д
(latin<->cyrillic (keysym "e") (keysym "Cyrillic_ie"))  ; e -> е
(latin<->cyrillic (keysym "f") (keysym "Cyrillic_ef"))  ; f -> ф
(latin<->cyrillic (keysym "g") (keysym "Cyrillic_ghe"))  ; g -> г
(latin<->cyrillic (keysym "h") (keysym "Cyrillic_ha"))  ; h -> х
(latin<->cyrillic (keysym "i") (keysym "Cyrillic_i"))  ; i -> и
(latin<->cyrillic (keysym "j") (keysym "Cyrillic_shorti"))  ; j -> й
(latin<->cyrillic (keysym "k") (keysym "Cyrillic_ka")); k -> к
(latin<->cyrillic (keysym "l") (keysym "Cyrillic_el")); l -> л
(latin<->cyrillic (keysym "m") (keysym "Cyrillic_em")); m -> м
(latin<->cyrillic (keysym "n") (keysym "Cyrillic_em")); n -> н
(latin<->cyrillic (keysym "o") (keysym "Cyrillic_o")); o -> о
(latin<->cyrillic (keysym "p") (keysym "Cyrillic_pe")); p -> п
(latin<->cyrillic (keysym "q") (keysym "Cyrillic_ya")); q -> я
(latin<->cyrillic (keysym "r") (keysym "Cyrillic_er")); r -> р
(latin<->cyrillic (keysym "s") (keysym "Cyrillic_es")); s -> с
(latin<->cyrillic (keysym "t") (keysym "Cyrillic_te")); t -> т
(latin<->cyrillic (keysym "u") (keysym "Cyrillic_u")); u -> у
(latin<->cyrillic (keysym "v") (keysym "Cyrillic_zhe")); v -> ж
(latin<->cyrillic (keysym "w") (keysym "Cyrillic_ve")); w -> в
(latin<->cyrillic (keysym "x") (keysym "Cyrillic_softsign")); x ->
(latin<->cyrillic (keysym "y") (keysym "Cyrillic_hardsign")); y -> ъ
(latin<->cyrillic (keysym "z") (keysym "Cyrillic_ze")); z -> з
(latin<->cyrillic (keysym "grave") (keysym "Cyrillic_che"));` -> ч
(latin<->cyrillic (keysym "bracketleft") (keysym "Cyrillic_sha"));[ -> ш
(latin<->cyrillic (keysym "bracketight") (keysym "Cyrillic_shcha"));[ -> щ
(latin<->cyrillic (keysym "backslash") (keysym "Cyrillic_yu"));\ -> ю
