;;;; -*- mode: lisp -*-
;;;; config.lisp --- StumpWM Configuration


(defvar swank-is-active nil)

(ql:quickload :cl-ppcre)
(ql:quickload :swank)
;;(ql:update-all-dists)

(unless swank-is-active
  (setf swank-is-active t)
  (require :swank)
  (swank-loader:init)
  (swank:create-server :port 4004
                       :style swank:*communication-style*
                       :dont-close t))

(in-package :stumpwm)

(defvar kal/config-directory
  (directory-namestring (concat
                         (namestring
                          (user-homedir-pathname)) ".config/stumpwm/config"))
  "The directory the config file is located.")

;;; From https://github.com/alezost/stumpwm-config/
(defun kal/load (filename)
  "Load a file FILENAME (without extension) from `kal/config-directory'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               kal/config-directory)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))


(set-font "-*-unifont-*-*-*-*-*-*-*-*-*-*-*")

(run-shell-command "xsetroot -cursor_name left_ptr" t)

(kal/load "keymap")
(kal/load "modeline")
(kal/load "help")
(kal/load "keytranslation")
(kal/load "keybindings")
(kal/load "layouts")
;(kal/load "minimum-interaction-mode")
;(kal/load "remap")

;;; Groups

(defcommand kal/set-up-groups () ()
            (grename "1")

            (mapcar #'(lambda (x)
                        (gnewbg (write-to-string x)))
                    (range 10 :min 2 :step 1))

            (gnewbg-float "0"))

(kal/set-up-groups)

;; Misc configuration

(set-msg-border-width 0)

(setf *mouse-focus-policy* :sloppy
      *ignore-wm-inc-hints* t
      *message-window-gravity* :center
      *message-window-input-gravity* :center
      *message-window-real-gravity* :center
      *message-window-input-gravity* :center
      *input-window-gravity* :center
      *normal-border-width* 0
      *window-border-style* :none
      *new-window-preferred-frame* '(:empty :focused))

;;(run-shell-command "setbg&!" t)

;;; config.lisp ends here
