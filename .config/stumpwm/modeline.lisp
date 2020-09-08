;;; modeline.lisp --- StumpWM configuration for the mode-line

(in-package :stumpwm)

(defun stumpwm::group-has-windows (group)
  "Format groups with windows differently from empty groups"
  (if (eq (current-group) group)
      (format nil "^[^5*^R")
      (if (group-current-window group)
          (format nil "^[^B^5*")
          (format nil "^[^1*")
          )))

(setf

 *mode-line-position* :bottom
 *mode-line-border-width* 0

 *window-info-format*
 (format nil "^>^B^5*%c ^b^6*%w^7*x^6*%h^7*~%%t")

 *window-format*
          " %15t "

 *group-formatters* (append *group-formatters*  '((#\w group-has-windows)))

 *group-format*
 "%w%t^]"

 *time-format-string-default*
 (format nil "^5*%H:%M:%S~%^2*%A~%^7*%d %B")

 *time-modeline-string* "%k:%M"
 *mode-line-timeout* 3
 *screen-mode-line-format*
 '(
   "%g | "                 ; group name
   "%v ^>"
   (:eval (run-shell-command "activeTask limit 30" t))
   (:eval (run-shell-command "echo -n \" $(livechart) |\"" t))
   (:eval (run-shell-command "echo -n \" ^[^B^5*$(mpdstatus)\"" t))
   (:eval (run-shell-command "echo -n \" ♬ $(pamixer --get-volume)% \"" t))
   "| ^[^5*%d^] | "                  ;time
   (:eval (run-shell-command "battery" t))
   ))

;;; Mode-line commands


(defvar *kal/mode-line-enabled* nil
  "State of the mode line, since StumpWM doesn't expose any way to query it")
(defvar *kal/stored-mode-line-state* nil
  "Stored state with kal/store-state-and-disable-mode-line")

(defun kal/set-mode-line (state)
  "Set mode line to specified state"
  (unless (eq *kal/mode-line-enabled* state)
    (setf *kal/mode-line-enabled* (not *kal/mode-line-enabled*))
    (mode-line)))

(defcommand kal/toggle-mode-line () ()
            (kal/set-mode-line (not *kal/mode-line-enabled*)))

(defcommand kal/enable-mode-line () ()
            (kal/set-mode-line t))

(defcommand kal/disable-mode-line () ()
            (kal/set-mode-line nil))

(defcommand kal/store-state-and-disable-mode-line () ()
            (setf *kal/stored-mode-line-state* *kal/mode-line-enabled*)
            (kal/disable-mode-line))

(defcommand kal/restore-mode-line-state () ()
            (kal/set-mode-line *kal/stored-mode-line-state*))

(kal/enable-mode-line)
