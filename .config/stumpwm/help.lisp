;;; help.lisp --- An "improved" version of the default help window

(require :cl-ppcre)
(in-package :stumpwm)

(defun create-or-append (a b list &key (test #'equalp))
  (if (assoc a list :test test)
      (push b (cdr (assoc a list :test test)))
      (push (cons a (list b)) list))
  list)

(defun flatten-alist (alist &key (test #'equalp))
  (let ((result nil))
    (dolist (el alist)
      (setf result (create-or-append (car el) (cdr el) result :test test)))
    result))

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
                 ,@body))
              *category-map*))

(defun category-of (command)
  (let ((category-maybe (some #'(lambda (elem) elem)
                              (map-categories (if (member command
                                                          entries
                                                          :test #'string=)
                                                  category
                                                  nil)))))
    (if category-maybe
        category-maybe
        nil)))

(defvar *suppress-commands-with-names* nil
  "Prevent commands with names in the list from showing in help.")

(setf *suppress-commands-with-names* nil)


(defun suppress-command (name)
  "Suppress the command with NAME from showing in help."
  (setf *suppress-commands-with-names*
        (cons name
              *suppress-commands-with-names*)))

(defun is-suppressed (name)
  "Return t if NAME is in `*suppress-commands-with-names*' and nil otherwise."
  (if (find name *suppress-commands-with-names* :test #'string=)
      t
      nil))

(defvar false-key-list nil
  "List of false keys.")

(setf false-key-list nil)

(defun define-false-key (map key name &optional category)
  "Define a false key in MAP bound to KEY executing NAME.
Keybindings defined this way will show up in help windows regardless of
the return value of `is-suppressed'"
  (setf false-key-list
        (create-or-append map
                          (cons key
                                name)
                          false-key-list))
  (when category
    (map-command-to-category name
                             category)))

(defun get-false-bindings (keymap)
  "Return the false keybindings registered in KEYMAP."
  (cdr (assoc keymap
              false-key-list)))

(defun format-key (binding)
  "Convert the `stumpwm::binding' BINDING to a more easy to manipulate form.
That is a cons cell consisting of the keybinding in a string form
acquired by `stumpwm::print-key', and the name of the command bound to
that key as returned by `name-of-command'."
  (let* ((key (stumpwm::binding-key binding))
         (key-name (stumpwm::print-key key))
         (command (princ-to-string (stumpwm::binding-command binding)))
         (command-name (name-of-command command)))
    (if (is-suppressed command-name)
        nil
        (cons key-name command-name))))


(defun parse-keymap (keymap)
  "Convert KEYMAP to a list of cons cells returned by `format-key'.
This also includes all the false bindings of KEYMAP as returned by
`get-false-bindings'."
  (let ((bindings (stumpwm::kmap-bindings keymap)))
    (remove-if #'null
               (append (mapcar #'format-key bindings)
                       (get-false-bindings keymap)))))

(defun separate-keymap-by-categories (keymap)
  "Separate the pairs of (key . command) in KEYMAP by the category of command."
  (flatten-alist (mapcar #'(lambda (key) (cons (category-of (cdr key)) key))
                         keymap)
                 :test #'string=))

(defun format-keybinding (binding-size-pair)
  "Converts BINDING-SIZE-PAIR to a string."
  (let* ((binding (car binding-size-pair))
         (size (cdr binding-size-pair))
         (spacing (apply #'concat (loop for n
                                        from (car size)
                                        below (+ 1 (cdr size))
                                        by 1
                                     collect " "))))
    (if (null (search "Cyrillic" (car binding)))
        (format nil
            "^[^5*~D^]~D~D~%"
            (car binding)
            spacing
            (cdr binding))
        "")))

(defun max-length (strings)
  "Return the size of the longest string in the list of STRINGS given."
  (apply #'max
         (mapcar #'(lambda (string) (length string))
                 strings)))

(defun repeat-char (char n)
  "Return a string that is CHAR repeated N times."
  (apply #'concat (loop for i
                        from 0
                        below n
                        by 1
                        collect char)))

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
         (max-size (if width
                       width
                       (max-length lines)))
         (bottom (repeat-char horizontal
                              max-size))
         (top (if category
                  (let* ((category-size (length category))
                         (rem-size (- (- max-size
                                         category-size)
                                      2))
                         (right (floor (/ rem-size 2)))
                         (left (if (< right (/ rem-size 2))
                                   (+ right 1)
                                   right)))
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

;;;;



;;(defclass panel ()
;;  ((title :initarg :title
;;          :type string
;;          :reader panel-title)
;;   (description :initarg :description
;;                :type string
;;                :reader panel-description)
;;   (content :type list
;;            :accessor panel-content)
;;   (content-count :type fixnum
;;                  :accessor panel-content-count)
;;   (width  :type fixnum
;;           :accessor panel-width)
;;   (height :type fixnum
;;           :accessor panel-height)))
;;
;;(defclass widget ()
;;  ((title :initarg :title :type string
;;          :reader widget-title)
;;   (content :type string
;;            :accessor widget-content)
;;   (width :type fixnum
;;          :accessor widget-width)
;;   (height :type fixnum
;;           :accessor widget-height)
;;   (foreground-color :type string
;;                     :initarg :fg
;;                     :accessor widget-fg-color)
;;   (background-color :type string
;;                     :initarg :bg
;;                     :accessor widget-bg-color)
;;   (title-foreground-color :type string
;;                           :initarg :title-fg
;;                           :accessor widget-title-fg-color)
;;   (title-background-color :type string
;;                           :initarg :title-bg
;;                           :accessor widget-title-bg-color)
;;   )
;;  (:default-initargs
;;   :fg "0"
;;   :bg "1"
;;   :title-fg "2"
;;   :title-bg "3"))
;;
;;(load "/home/kalelzar/Code/lisp/dash.lisp/package.lisp")
;;(load "/home/kalelzar/Code/lisp/dash.lisp/sequential.lisp")
;;(load "/home/kalelzar/Code/lisp/dash.lisp/dash.lisp")
;;
;;(defun available-width ()
;;  (screen-width (current-screen)))
;;
;;(defun available-height ()
;;  (screen-height (current-screen)))
;;
;;(defun available-width-chars ()
;;  (floor
;;   (/ (available-width)
;;      (xlib:font-property (screen-font (current-screen))
;;                          :quad_width))))
;;(defun available-height-chars ()
;;  (floor
;;   (/ (available-height)
;;      (font-height (screen-font (current-screen))))))
;;
;;(defclass keybind-widget (widget)
;;  ((keybindings :initarg keybindings :type list)))
;;
;;
;;(defmethod widget-print :before ((widget widget))
;;  (widget-refresh widget))
;;
;;(defmethod widget-print :after ((widget widget))
;;  (prin1-to-string (widget-content widget)))
;;
;;(defmethod widget-print ((widget widget))
;;  (widget-box widget))
;;
;;(defmethod widget-box ((widget widget))
;;  (let ((horizontal (repeat-char "-" (- (widget-width widget) 2))))
;;    (setf (widget-content widget)
;;          (format nil "~D~%~D~D"
;;                  horizontal
;;                  (dash:-reduce #'concat
;;                                (dash:--map (format nil "|~D|~%" dash:it)
;;                                            (cl-ppcre:split #\Newline
;;                                                            (widget-content widget))))
;;                  horizontal))
;;    ))
;;
;;(widget-print (make-instance 'keybind-widget
;;               :title "Help"))
;;
;;(defclass rich-kmap ()
;;  ((kmap :type stumpwm::kmap
;;         :initarg :kmap
;;         :reader kmap-inner)
;;   (name :type string
;;         :initarg :name
;;         :reader kmap-name)
;;   (description :type string
;;                :initarg :description
;;                :reader kmap-description)
;;   (bindings :type list
;;             :accessor kmap-rich-bindings
;;             :initform nil)))
;;
;;(defun new-rich-kmap (name description)
;;  (make-instance 'rich-kmap
;;                 :kmap (stumpwm::make-kmap)
;;                 :name name
;;                 :description description))
;;
;;(defclass rich-binding ()
;;  ((key :type stumpwm::key
;;        :initarg :key
;;        :reader rich-binding-key)
;;   (command :type string
;;            :initarg :command
;;            :reader rich-binding-command)
;;   (description :type string
;;                :initarg :description
;;                :reader rich-binding-description)
;;   (category :type string
;;             :initarg :category
;;             :reader rich-binding-category)
;;   (suppress :type boolean
;;             :initarg :suppress
;;             :accessor rich-binding-supressed-p))
;;  (:default-initargs
;;   :category ""
;;   :suppress nil))
;;
;;
;;(defmethod bind-key-2 ((keymap rich-kmap)
;;                       (key string)
;;                       (command string)
;;                       (description string)
;;                       (category string)
;;                       &optional suppress)
;;  (define-key (kmap-inner keymap)
;;              (kbd key)
;;    command)
;;  (setf (kmap-rich-bindings keymap)
;;        (append (kmap-rich-bindings keymap)
;;                (list (make-instance 'rich-binding
;;                                     :key (kbd key)
;;                                     :command command
;;                                     :category category
;;                                     :description description
;;                                     :suppress suppress)))))
;;
;;(defmethod bind-key-2 ((keymap rich-kmap)
;;                       (key string)
;;                       (command symbol)
;;                       (description string)
;;                       (category string)
;;                       &optional suppress)
;;  (define-key (kmap-inner keymap)
;;              (kbd key)
;;    command)
;;  (setf (kmap-rich-bindings keymap)
;;        (append (kmap-rich-bindings keymap)
;;                (list (make-instance 'rich-binding
;;                                     :key (kbd key)
;;                                     :command command
;;                                     :category category
;;                                     :description description
;;                                     :suppress suppress)))))
;;
;;(defvar *rich-root-map* (new-rich-kmap "Root Map" "Base prefix"))
;;
;;
;;
;;(bind-key-2 *rich-root-map*
;;            "a"
;;            "time"
;;            "Show date/time"
;;            "System")
;;(bind-key-2 *rich-root-map*
;;            "H-a"
;;            "exec rofi ..."
;;            "Task switcher"
;;            "Windows")
;;(bind-key-2 *rich-root-map*
;;            "H-Cyrillic_a"
;;            "exec rofi ..."
;;            "Task switcher"
;;            "Windows"
;;            t)
;;(bind-key-2 *rich-root-map*
;;            "o"
;;            '*rich-quickopen-map*
;;            "Quick Open"
;;            "System")
;;
;;
;;
;;(defmethod get-labeled-list ((keymap rich-kmap))
;;  (sequential:with-list
;;    (stumpwm::kmap-rich-bindings keymap)
;;  (dash:--filter (not (rich-binding-supressed-p dash:it)))
;;  (dash:--map
;;   (list :key     (stumpwm::print-key (rich-binding-key dash:it))
;;         :key-length (length (stumpwm::print-key (rich-binding-key dash:it)))
;;         :description (rich-binding-description dash:it)
;;         :description-length (length (rich-binding-description dash:it))))))
;;
;;(let* ((labeled-list
;;         (sequential:with-list
;;             (stumpwm::kmap-rich-bindings *rich-root-map*)
;;           (dash:--filter (not (rich-binding-supressed-p dash:it)))
;;           (dash::--group-by
;;            (rich-binding-category dash:it))
;;           (dash:--map
;;            (cons (car dash:it)
;;                  (sequential:with-list (cdr dash:it)
;;                    (dash:--map
;;                     (list
;;                      :key     (stumpwm::print-key (rich-binding-key dash:it))
;;                      :key-length (length (stumpwm::print-key (rich-binding-key dash:it)))
;;                      :description (rich-binding-description dash:it)
;;                      :description-length (length (rich-binding-description dash:it))))))
;;            )
;;           (dash:--map
;;            (list :category
;;                  (car dash:it)
;;                  :max-key-length
;;                  (sequential:with-list (cdr dash:it)
;;                    (dash:--fold-left 0
;;                                      (max dash:acc (getf dash:it :key-length))))
;;                  :max-description-length
;;                  (sequential:with-list (cdr dash:it)
;;                    (dash:--fold-left 0
;;                                      (max dash:acc (getf dash:it :description-length))))
;;                  :bindings
;;                  (cdr dash:it))))))
;;  (sequential:with-list labeled-list
;;    (dash:--map
;;     (let ((max-key-length (getf dash:it :max-key-length))
;;           (max-description-length (getf dash:it :max-description-length)))
;;       (list (getf dash:it :category)
;;             (sequential:with-list (getf dash:it :bindings)
;;               (dash:--map
;;                (list
;;                 (format nil
;;                         "~D~D"
;;                         (getf dash:it :key)
;;                         (repeat-char " "  (- max-key-length
;;                                              (getf dash:it :key-length))))
;;                 (format nil
;;                         "~D~D"
;;                         (getf dash:it :description)
;;                         (repeat-char " "  (- max-description-length
;;                                              (getf dash:it :description-length))))))
;;               )))
;;     )))
;;
;;
;;(defmethod widget-refresh ((widget keybind-widget))
;;  (setf (widget-content widget) (format-keymap '*root-map*))
;;  (setf (widget-width widget) 80))


