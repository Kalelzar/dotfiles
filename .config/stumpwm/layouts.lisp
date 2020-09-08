;;; layouts.lisp --- Layouts for StumpWM

(in-package :stumpwm)

(defvar layouts nil
  "Layout map")

;(setf layouts nil)

(defun register-layout (name fancy-name keybinding &rest windows)
  (redefine-key *layout-map* (kbd keybinding)
                (concat "open-layout " name) fancy-name)
  (setf layouts (acons name windows layouts))
  )

(register-layout "mp" "Music Player" "m"
                 (kal/execterm "ncmpcpp")
                 "exec sxiv -b -s f $HOME/.cache/songthumb"
                 (kal/execterm "cava")
                 )

(defcommand open-layout (layout) ((:string "Layout: "))
            (dolist (window (cdr (assoc layout layouts :test #'string=)))
              (run-commands window))
            (restore-from-file layout))
