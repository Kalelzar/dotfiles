(in-package #:stumpwm)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(setf *available-keyboard-layouts* '(("us" . nil) ("bg" . "phonetic")))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun cycle-layout (&optional (direction :forward))
  "Cycle the list of layouts in the provided DIRECTION.
DIRECTION can be either :forward (which is the default) or :backward."
    (cond
      ((eq direction :forward)
       (let ((head (car *available-keyboard-layouts*))
             (tail (cdr *available-keyboard-layouts*)))
         (setf *available-keyboard-layouts* (append tail (list head)))))
      ((eq direction :backward)
       (let ((init (butlast *available-keyboard-layouts*))
             (last (last *available-keyboard-layouts*)))
         (setf *available-keyboard-layouts* (append last init))))
      (t (error "Invalid argument"))))

(defun set-layout (&optional (layout nil))
  "Use `setxkbmap' to change the current keyboard LAYOUT.
If LAYOUT is nil then use the car of `*available-keyboard-layouts*'.
If LAYOUT is a string then use that as an argument to `setxkbmap'.
If LAYOUT is a cons cell of two strings then use the car of the cell as the
'language' and the cdr as 'layout'"
  (typecase layout
    (cons
     (let* ((lang (car layout))
           (form (or (cdr layout) ""))
           (command-format "setxkbmap ~D ~D; rebindKeys")
           (command (format nil command-format lang form)))
       (run-shell-command command t)))
    (string (set-layout (cons layout nil)))
    (null (set-layout (car *available-keyboard-layouts*)))
    (t "error")))

(defun current-keyboard-layout (ml)
  "Return current keyboard layout"
  (declare (ignore ml))
  (let ((layout (car *available-keyboard-layouts*)))
    (if (null (cdr layout))
        (format nil "~D" (car layout))
        (format nil "~D (~D)" (car layout) (cdr layout)))))

;; Make the current keyboard layout accessible from the modeline via %L.
(add-screen-mode-line-formatter #\L #'current-keyboard-layout)

;;;;;;;;;;;;;;
;; Commands ;;
;;;;;;;;;;;;;;


(defcommand switch-layout () ()
  (cycle-layout)
  (set-layout)
  (message "Current layout: '~D'" (current-keyboard-layout nil)))

;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;

(define-key *top-map* (kbd "s-SPC") "switch-layout")
