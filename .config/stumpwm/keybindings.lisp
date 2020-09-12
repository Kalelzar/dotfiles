;;; keybindings.lisp --- StumpWM key bindings

(in-package :stumpwm)

(export '(*bg-map* *headphone-map* *layout-map*))



(defun range (max &key (min 0) (step 1))
  "Get a list of integers."
  (loop for n from min below max by step
        collect n))

(defun redefine-key (map key command &optional name category)
  (undefine-key map key)
  (define-key map key command)
  (when name (map-command-to-name command name))
  (when category (map-command-to-category name category))
  )

(map-command-to-category "Prefix key" "Keymap")

(defun kal/execterm (cmd)
  "Start a terminal application"
  (concat (concat "exec $TERMINAL -e '" cmd) "'"))

(defcommand kal/print-output (cmd) ()
  (let ((v (run-shell-command cmd t)))
  (message v)
  v))

(defun kal/gen-print-output (cmd)
  "Print the output of a shell command."
  (concat "eval (run-shell-command \""
          (concat cmd "| sed -E 's/(.*)/Brightness: \1/g' | tr -d '\n' \" t)")))

(defvar *directions*
  '("left" "right" "up" "down") "Cardinal directions")

(defun kal/register-frame-move-key (dir type map prefix)
  "Map the given DIR key to moving TYPE in the same direction to MAP."
  (redefine-key map
      (kbd (concat prefix (string-capitalize dir)))
      (format nil "move-~D ~D" type dir)
      (format nil "Move ~D ~D" type dir) "Frame"))

(defun stumpwm::window-is-visible (window)
  "Format visible windows differently from hidden windows"
  (if (eq (current-window) window)
      (format nil "^[")
      (if (window-visible-p window)
          (format nil "^[^b^1*")
          (format nil "^[^5*")
          )))

(setf *window-formatters* (append *window-formatters* '((#\f window-is-visible))))

(defcommand vpull-hidden-next () ()
  (if (null (current-window))
      (pull-hidden-next)
      (unless (null (remove-if #'(lambda (window)
                                   (window-visible-p window))
                               (group-windows (current-group))))
        (pull-hidden-next)
        (echo-windows "%f%t^]")
        (sleep 0.5))))


(defcommand vpull-hidden-previous () ()
  (if (null (current-window))
      (pull-hidden-previous)
      (unless (null (remove-if #'(lambda (window)
                                   (window-visible-p window))
                               (group-windows (current-group))))
        (pull-hidden-previous)
        (echo-windows "%f%t^]")
        (sleep 0.5))))

;;Rebind groups to prefix number
(mapcar #'(lambda (x)
            (redefine-key *top-map* (kbd (concat "H-" (write-to-string x)))
                          (format nil "gselect ~D" x)
                          (format nil "Select group ~D" x))
            (suppress-command (format nil "Select group ~D" x))
            (redefine-key *top-map* (kbd (concat "H-s-" (write-to-string x)))
                          (format nil "gmove ~D" x)
                          (format nil "Move to group ~D" x))
                    (suppress-command (format nil "Move to group ~D" x)))
        (range 10 :min 0 :step 1))

(define-false-key *top-map* "H-<n>" "Select group <n>" "Group")
(define-false-key *top-map* "H-s-<n>" "Move to group <n>" "Group")



;;; Set prefix key
(set-prefix-key (kbd "H-a"))

;; Unbind all default keys

(setf stumpwm::*root-map* (stumpwm:make-sparse-keymap))
(setf stumpwm::*group-root-map* (stumpwm:make-sparse-keymap))
(setf stumpwm::*float-group-root-map* (stumpwm:make-sparse-keymap))
(setf stumpwm::*tile-group-root-map* (stumpwm:make-sparse-keymap))

;;; Restore defaults

(redefine-key *root-map* (kbd "C-g") "abort" "Abort" "System")
(redefine-key *root-map* (kbd "k") "delete" "Close window" "Frame")
(redefine-key *root-map* (kbd "a") "time" "Time" "System")
(redefine-key *root-map* (kbd ";") "colon" "Run StumpWM Command" "System")
(redefine-key *root-map* (kbd "r") "iresize" "Resize" "Frame")


;;; Keymaps

(defvar *layout-map* (make-sparse-keymap)
  "Keymap for common window layouts.")

(redefine-key *root-map* (kbd "l") '*layout-map* "Layouts" "Keymap")

(defvar *headphone-map* (make-sparse-keymap)
  "Key map for audio controls from headphones")

(redefine-key *top-map* (kbd "XF86AudioMute") '*headphone-map*
              "Headphone controls" "Keymap")

(defvar *bg-map* (make-sparse-keymap)
  "Key map for background controls")

(redefine-key *root-map* (kbd "B") '*bg-map*
              "Background controls" "Keymap")

;;; Help

(redefine-key *layout-map* (kbd "h") "show-keymap *layout-map*" "Help" "System")
(redefine-key *root-map* (kbd "h") "show-keymap *root-map*" "Help" "System")
(redefine-key *top-map* (kbd "H-h") "show-keymap *top-map*" "Help" "System")
(redefine-key *headphone-map* (kbd "h")
              "show-keymap *headphone-map*"
              "Help"
              "System")
(redefine-key *bg-map* (kbd "h") "show-keymap *bg-map*" "Help" "System")

;;; Common commands

(redefine-key *top-map* (kbd "H-RET") "exec st" "Terminal" "App")
(redefine-key *root-map* (kbd "RET") "exec" "Run command" "System")
(redefine-key *top-map* (kbd "H-w") "exec firefox" "Firefox" "App")

(redefine-key *top-map* (kbd "H-e") "exec emacsvi" "Emacs" "App")
(redefine-key *top-map* (kbd "H-m") (kal/execterm "ncmpcpp") "Music player" "App")
(redefine-key *top-map* (kbd "H-n") (kal/execterm "newsboat")
              "RSS Feeds" "App")
(redefine-key *top-map* (kbd "H-p") "exec mpc toggle"
              "Toggle play/pause" "Music controls")
(redefine-key *top-map* (kbd "H-.") "exec mpc next"
              "Next song" "Music controls")
(redefine-key *top-map* (kbd "H-,") "exec mpc prev"
              "Prev song" "Music controls")
(redefine-key *top-map* (kbd "H-r") (kal/execterm "lfrun")
              "File manager" "App")

(redefine-key *top-map* (kbd "H-f") "fullscreen" "Fullscreen" "Frame")

;; Audio related controls
(redefine-key *top-map* (kbd "H-=") "exec pamixer -i 2 && volumeosd"
              "Volume +2" "System")
(redefine-key *top-map* (kbd "H--") "exec pamixer -d 2 && volumeosd"
              "Volume -2" "System")
(redefine-key *top-map* (kbd "H-M") "exec pamixer -t  && volumeosd"
              "Toggle mute" "System")
(redefine-key *top-map* (kbd "XF86AudioRaiseVolume")
              "exec pamixer -i 2 && volumeosd"
              "Volume +2" "System")
(redefine-key *top-map* (kbd "XF86AudioLowerVolume")
              "exec pamixer -d 2 && volumeosd"
              "Volume -2" "System")

;; Frame controls
(redefine-key *top-map* (kbd "H-SPC") "vpull-hidden-next"
              "Next window on stack" "Frame")
(redefine-key *top-map* (kbd "H-S-SPC") "vpull-hidden-previous"
              "Prev window on stack" "Frame")


(dolist (dir *directions*)
  (kal/register-frame-move-key dir "focus" *top-map* "H-")
  (kal/register-frame-move-key dir "window" *root-map* ""))

(redefine-key *root-map* (kbd "f") "float-this" "Float window" "Floating")
(redefine-key *root-map* (kbd "F") "unfloat-this" "Tile window" "Floating")
(redefine-key *root-map* (kbd "H-f") "flatten-floats"
              "Tile all windows" "Floating")


;; Use the default Emacs keybindings for frames
(redefine-key *root-map* (kbd "1") "only" "Remove all other frames" "Frame")
(redefine-key *root-map* (kbd "2") "vsplit" "Split vertically" "Frame")
(redefine-key *root-map* (kbd "3") "hsplit" "Split horizontally" "Frame")
(redefine-key *root-map* (kbd "0") "remove" "Remove current frame" "Frame")

;;; Uncommon commands

(redefine-key *root-map* (kbd "b") "kal/toggle-mode-line" "Toggle mode-line"
              "System")
(redefine-key *root-map* (kbd "t") "exec torwrap"
              "Torrents"
              "App")
(redefine-key *root-map* (kbd "s") "exec currentlyplaying"
              "Current song"
              "Music controls")
(redefine-key *root-map* (kbd "T") (kal/execterm "htop")
              "System monitor"
              "App")
(redefine-key *root-map* (kbd "H-s") "exec maim -i \"$(xdotool getactivewindow)\" pic-window-\"$(date '+%y%m%d-%H%M-%S').png\""
              "Screenshot window"
              "System")

(redefine-key *root-map* (kbd "w") "windows %f%n %t^]"
              "Show windows"
              "Frame")

(redefine-key *root-map* (kbd "R") "loadrc" "Reload config file." "System")

;; Headphone controls
(redefine-key *headphone-map* (kbd "XF86AudioRaiseVolume") "exec mpc next"
              "Next song" "Music controls")
(redefine-key *headphone-map* (kbd "XF86AudioLowerVolume") "exec mpc prev"
              "Prev song" "Music controls")
(redefine-key *headphone-map* (kbd "XF86AudioMute") "exec pamixer -t && volumeosd"
              "Mute/unmute" "System")
(redefine-key *headphone-map* (kbd "H-p") "exec mpc toggle"
              "Pause/unpause"
              "Music controls")
(redefine-key *headphone-map* (kbd "a") "exec mpdannounce"
              "Announce song" "Music controls")




;; Background controls
(redefine-key *bg-map* (kbd "b") "exec banbackground"
              "Ban background")
(redefine-key *bg-map* (kbd "n")  "exec setbg \"$HOME/Backgrounds\""
              "Next background")

;; Backlight

(redefine-key *top-map* (kbd "XF86MonBrightnessUp") "exec xbacklight +5"
              "Backlight +5" "System")
(redefine-key *top-map* (kbd "XF86MonBrightnessDown") "exec xbacklight -5"
              "Backlight -5" "System")

; Larger Increments

(redefine-key *top-map* (kbd "s-XF86MonBrightnessUp") "exec xbacklight +20"
              "Backlight +20" "System")
(redefine-key *top-map* (kbd "s-XF86MonBrightnessDown") "exec xbacklight -20"
              "Backlight -20" "System")


;(redefine-key *top-map* (kbd "C-XF86MonBrightnessUp")
;              (kal/gen-print-output "xbacklight")
;              "Do nothing")

; Alternate Bindings for keyboards without dedicated brightness keys

(redefine-key *root-map* (kbd "=") "exec xbacklight +10"
              "Backlight +10" "System")
(redefine-key *root-map* (kbd "-") "exec xbacklight -10"
              "Backlight -10" "System")
