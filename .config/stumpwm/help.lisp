;;; help.lisp --- An "improved" version of the default help window

(require :cl-ppcre)
(in-package :stumpwm)

(defun create-or-append (a b list &key (test #'equalp))
  (if (assoc a list :test test)
      (push b (cdr (assoc a list :test test)))
      (push (cons a (list b)) list)
      )
  list
  )

(defun flatten-alist (alist &key (test #'equalp))
  (let ((result nil))
    (dolist (el alist)
      (setf result (create-or-append (car el) (cdr el) result :test test)))
    result
    )
  )

(defvar *command-to-fancy-name-map* nil
  "Maps a command to a more human-readable name.")

(setf *command-to-fancy-name-map* nil)

(defun map-command-to-name (command name)
  "Associate COMMAND with a NAME."
  (typecase command
    (string (setf *command-to-fancy-name-map*
                  (acons command name *command-to-fancy-name-map*)))
    (symbol (setf *command-to-fancy-name-map*
                  (acons (symbol-name command) name *command-to-fancy-name-map*)))
    ))

(map-command-to-name '*root-map* "Prefix key")

(defun name-of-command (command)
  "Return the name associated with COMMAND."
  (let ((name (cdr (assoc command *command-to-fancy-name-map* :test #'equalp))))
    (if name name command)))


(defvar *category-map* nil
  "List of which commands belong to which category.")

(setf *category-map* nil)


(defun map-command-to-category (command category)
  (typecase command
    (string (setf *category-map*
                  (create-or-append category
                                    command
                                    *category-map*)))
    (symbol (setf *category-map*
                  (create-or-append category
                                    (symbol-name command)
                                    *category-map*)))))

(defmacro map-categories (&body body)
  `(mapcar #'(lambda (category-entry)
               (let ((category (car category-entry))
                     (entries  (cdr category-entry))
                     )
                 ,@body
                 ))
              *category-map*
              ))

(defun category-of (command)
  (let ((category-maybe (some #'(lambda (elem) elem)
                              (map-categories (if (member command
                                                          entries
                                                          :test #'string=)
                                                  category
                                                  nil))
                              )))
    (if category-maybe
        category-maybe
        nil)
    )
  )

(defvar *suppress-commands-with-names* nil
  "Prevent commands with names in the list from showing in help.")

(setf *suppress-commands-with-names* nil)


(defun suppress-command (name)
  "Suppress the command with NAME from showing in help."
  (setf *suppress-commands-with-names* (cons name *suppress-commands-with-names*)))

(defun is-suppressed (name)
  "Return t if NAME is in `*suppress-commands-with-names*' and nil otherwise."
  (if (find name *suppress-commands-with-names* :test #'string=)
      t
      nil))

(defvar false-key-list nil "List of false keys.")

(setf false-key-list nil)

(defun define-false-key (map key name &optional category)
  "Define a false key in MAP bound to KEY executing NAME.
   Keybindings defined this way will show up in help windows regardless of
   the return value of `is-suppressed'"
  (setf false-key-list (create-or-append map (cons key name) false-key-list))
  (when category (map-command-to-category name category))
  )

(defun get-false-bindings (keymap)
  "Return the false keybindings registered in KEYMAP."
  (cdr (assoc keymap false-key-list)))

(defun format-key (binding)
  "Convert the `stumpwm::binding' BINDING to a more easy to manipulate form.
   This is a cons cell consisting of the keybinding in a string form
   acquired by `stumpwm::print-key', and the name of the command bound to
   that key as returned by `name-of-command'."
  (let* ((key (stumpwm::binding-key binding))
         (key-name (stumpwm::print-key key))
         (command (princ-to-string (stumpwm::binding-command binding)))
         (command-name (name-of-command command)))
    (if (is-suppressed command-name)
        nil
        (cons key-name command-name))
    ))


(defun parse-keymap (keymap)
  "Convert KEYMAP to a list of cons cells returned by `format-key'.
   This also includes all the false bindings of KEYMAP as returned by
`get-false-bindings'."
  (let ((bindings (stumpwm::kmap-bindings keymap)))
    (remove-if #'null (append (mapcar #'format-key bindings) (get-false-bindings keymap)))))

(defun separate-keymap-by-categories (keymap)
  "Separate the pairs of (key . command) in KEYMAP by the category of command."
  (flatten-alist (mapcar #'(lambda (key)
                             (cons (category-of (cdr key)) key))
                         keymap)
                 :test #'string=)
  )

(defun format-keybinding (binding-size-pair)
  "Converts BINDING-SIZE-PAIR to a string."
  (let* ((binding (car binding-size-pair))
         (size (cdr binding-size-pair))
         (spacing (apply #'concat (loop for n from (car size) below (+ 1 (cdr size)) by 1 collect " "))))
      (format nil "^[^5*~D^]~D~D~%" (car binding) spacing (cdr binding)))
)

(defun max-length (strings)
  "Return the size of the longest string in the list of STRINGS given."
  (apply #'max (mapcar #'(lambda (string) (length string)) strings))
  )

(defun repeat-char (char n)
  "Return a string that is CHAR repeated N times."
  (apply #'concat (loop for i from 0 below n by 1 collect char))
  )

(defun wrap-border (text &key
                           (category nil)
                           (width 0)
                           (vertical "┃")
                           (horizontal "━")
                           (top-left "┏")
                           (top-right "┓")
                           (bottom-left "┗")
                           (bottom-right "┛"))
  "Surround the given TEXT with a border and CATEGORY."
  (let* ((lines (cl-ppcre:split #\newline
                                text))
         (max-size (if width width (max-length lines)))
         (bottom (repeat-char horizontal
                              max-size))
         (top (if category
                  (let* ((category-size (length category))
                         (rem-size (- (- max-size
                                         category-size)
                                      2))
                         (right (floor (/ rem-size 2)))
                         (left (if (< right (/ rem-size 2))
                                   (+ right 1) right)))
                    (format nil
                            "~D┫~D┣~D"
                            (repeat-char horizontal
                                         right)
                            category
                            (repeat-char horizontal
                                         left)))
                  bottom)))
    (format nil
            "~D~D~D~%~D~%~D~D~D~%"
            top-left
            top
            top-right
            (apply #'concat
                   (mapcar #'(lambda (line)
                       (let ((padding (repeat-char " "
                                                   (- max-size (- (length line) 7))))
                             (line (cl-ppcre:regex-replace-all "^"
                                                               line vertical)))
                         (cl-ppcre:regex-replace-all "$"
                                                     line
                                                     (format nil
                                                             "~D~D~%"
                                                             padding
                                                             vertical))))
                           lines ))
            bottom-left
            bottom
            bottom-right)))

(defun format-keymap-category (category-entries max-keychord-size max-size)
  (let* ((category (car category-entries))
         (keymap (cdr category-entries))
         (keymap-with-length (mapcar
                              #'(lambda (binding)
                                  (cons binding
                                        (cons (length (car binding))
                                              max-keychord-size)))
                              keymap)))
    (wrap-border (apply #'concat
                        (mapcar #'format-keybinding
                                (sort keymap-with-length
                                      #'(lambda (a b)
                                          (string< (cdr (car a))
                                                   (cdr (car b)))))))
                 :width max-size
                 :category category)))

(defun format-keymap (raw-keymap)
  "Formats RAW-KEYMAP to a help string."
  (let* ((keymap (parse-keymap (eval raw-keymap)))
         (prefix (stumpwm::print-key-seq (reverse (cdr *current-key-seq*))))
         (max-keychord-size
          (max-length
                 (mapcar
                  #'(lambda (binding)
                      (car binding))
                  keymap)))
         (max-command-size
          (max-length
                 (mapcar
                  #'(lambda (binding)
                      (cdr binding))
                  keymap)))
         (max-size (+ 1 (+ max-keychord-size max-command-size))))
    (concat (if (not (<= (length prefix) 5))
                (format nil
                        "Keymap: ^[^5*~D^]~%Prefix: ^[^5*~D^]~%"
                        (symbol-name raw-keymap)
                        prefix)
                (format nil
                        "Keymap: ^[^5*~D^]~%"
                        (symbol-name raw-keymap)
                        ))
            (apply #'concat
                   (mapcar #'(lambda (category-keymap)
                               (format-keymap-category category-keymap
                                                       max-keychord-size
                                                       max-size))
                           (separate-keymap-by-categories keymap))))))

(defcommand show-keymap (map) ((:variable "Key map: "))
  (let ((*suppress-echo-timeout* t)
        (*message-window-gravity* :bottom-right))
              (message (format-keymap map))))
