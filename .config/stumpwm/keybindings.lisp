;;; keybindings.lisp --- StumpWM key bindings

(in-package :stumpwm)

(export '(*bg-map* *headphone-map* *layout-map* *open-map*))

(defun range (max &key (min 0) (step 1))
  "Get a list of integers."
  (loop for n from min below max by step
        collect n))


(defun redefine-key (map key command &optional name category)
  (undefine-key map key)
  (define-key map key command)
  (when name (map-command-to-name command name))
  (when category (map-command-to-category name category)))

(defun redefine-key-with-alternatives (map key command &optional name category)
  (dolist (alternative (key-alternatives key))
    (redefine-key map alternative command name category)))

(map-command-to-category "Prefix key" "Keymap")

(defun kal/execterm (cmd)
  "Start a terminal application"
  (concat (concat "exec $TERMINAL -e '" cmd) "'"))


(defcommand exec-in-terminal (command) ((:shell "Command"))
    "Evaluate the given shell COMMAND inside a temporary terminal window.
The terminal used is the one pointed to by the TERMINAL environment variable."
  (run-shell-command (format nil
                             "$TERMINAL -e '~D'"
                             command)))

(defcommand watch-link-from-clipboard () ()
  (run-or-raise "mpv --ytdl-format='bestvideo[height<=1080][fps<=30]+bestaudio' \"$(xclip -o)\""
                '(:class "mpv")))

;; (gnext)

(defcommand kal/print-output (cmd) ((:shell "Command"))
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

(defun window-floating-p (window)
  (type= (type-of window) 'float-window))

(defun stumpwm::window-is-visible (window)
  "Format visible windows differently from hidden windows"
  (if (eq (current-window) window)
      (format nil "^[")
      (if (window-visible-p window)
          (if (window-floating-p window)
              (format nil "^[^B^5*")
              (format nil "^[^b^1*")
              )
          (format nil "^[^5*")
          )))

(setf *window-formatters* (append *window-formatters* '((#\f window-is-visible))))

(defun change-pulseaudio-volume (volume-delta)
  (let ((command (concat "pulsemixer "
                         (cond
                           ((zerop volume-delta) "--toggle-mute --get-volume")
                           ((positive-integer-p volume-delta)
                            (format nil
                                    "--change-volume +~D --get-volume"
                                    volume-delta))
                           ((negative-integer-p volume-delta)
                            (format nil
                                    "--change-volume -~D --get-volume"
                                    (abs volume-delta)))))))
    (message "Volume ~D" (run-shell-command command t))))

(defcommand pulse-volume-change (by) (:number "Change by")
  (change-pulseaudio-volume by))

(defcommand pulse-volume-increase () ()
  (pulse-volume-change 2))

(defcommand pulse-volume-decrease () ()
  (pulse-volume-change -2))

(defcommand pulse-toggle-mute () ()
  (pulse-volume-change 0))

(defun cycle-windows-forward ()
  (let* ((windows (group-windows (current-group)))
         (windows-for-cycle
          (remove-if-not #'(lambda (window)
                             (or (equalp window (current-window))
                                 (window-hidden-p window)
                                 (equalp (type-of window) 'stumpwm::float-window)))
               windows))
         (at (position (current-window) windows-for-cycle))
         (next (if (>= at
                       (- (list-length windows-for-cycle) 1))
                   (nth 0 windows-for-cycle)
                   (nth (+ 1 at) windows-for-cycle))))
    (message "~D" at)
    (unless (or (= 1 (list-length windows-for-cycle)) (null next))
      (typecase next
        (stumpwm::tile-window (pull-window next))
        (stumpwm::float-window (progn
                                 (really-raise-window next)
                                 (focus-window next))))
      )
    ))

(defun cycle-windows-backwards ()
  (let* ((windows (group-windows (current-group)))
         (windows-for-cycle
          (remove-if-not #'(lambda (window)
                             (or (equalp window (current-window))
                                 (window-hidden-p window)
                                 (equalp (type-of window) 'stumpwm::float-window)))
               windows))
         (at (position (current-window) windows-for-cycle))
         (previous (if (<= at 0)
                   (nth (- (list-length windows-for-cycle) 1) windows-for-cycle)
                   (nth (- 1 at) windows-for-cycle))))
    (message "~D" at)
    (unless (or (= 1 (list-length windows-for-cycle)) (null previous))
      (typecase previous
        (stumpwm::tile-window (pull-window previous))
        (stumpwm::float-window (progn
                                 (really-raise-window previous)
                                 (focus-window previous))))
      )
    ))


(defcommand really-float-this () ()
  (unless (window-floating-p (current-window))
    (float-this)
    (toggle-always-on-top)
    (float-window-move-resize (current-window)
                              :width 480
                              :height 270)))

(defcommand vpull-hidden-next () ()
  (let ((windows-in-group (group-windows (current-group))))
    (unless (null windows-in-group)
      (if (null (current-window))
          (pull-hidden-next)
          (unless (null (remove-if #'(lambda (window)
                                       (or (window-visible-p window)
                                           (window-floating-p window)
                                           (and (string= (window-class window)
                                                         "St")
                                                (string/= (window-title window)
                                                          "st"))))
                                       windows-in-group))
            (pull-hidden-next)
            (echo-windows "%f%t^]")
            (sleep 0.5))))))

(defcommand vpull-hidden-previous () ()
  (let ((windows-in-group (group-windows (current-group))))
    (when (not (null windows-in-group))
      (if (null (current-window))
          (pull-hidden-previous)
          (unless (null (remove-if #'(lambda (window)
                                       (window-visible-p window))
                                   windows-in-group))
            (pull-hidden-previous)
            (echo-windows "%f%t^]")
            (sleep 0.5))))))

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
(redefine-key-with-alternatives *top-map* (kbd "H-a") '*root-map*
                                "Prefix key" "Keymap")

;; Unbind all default keys

(setf stumpwm::*root-map* (stumpwm:make-sparse-keymap))
(setf stumpwm::*group-root-map* (stumpwm:make-sparse-keymap))
(setf stumpwm::*float-group-root-map* (stumpwm:make-sparse-keymap))
(setf stumpwm::*tile-group-root-map* (stumpwm:make-sparse-keymap))

;;; Restore defaults

(redefine-key-with-alternatives *root-map* (kbd "C-g") "abort" "Abort" "System")
(redefine-key-with-alternatives *root-map* (kbd "k") "delete" "Close window"
                                "Frame")
(redefine-key-with-alternatives *root-map* (kbd "a") "time" "Time" "System")
(redefine-key *root-map* (kbd ";") "colon" "Run StumpWM Command" "System")
(redefine-key-with-alternatives *root-map* (kbd "r") "iresize" "Resize" "Frame")
(redefine-key *root-map* (kbd "=") "balance-frames" "Balance Frames" "Frame")



;;; Keymaps

(defvar *layout-map* (make-sparse-keymap)
  "Keymap* for common window layouts.")

(redefine-key-with-alternatives *root-map* (kbd "l") '*layout-map* "Layouts" "Keymap")

(defvar *headphone-map* (make-sparse-keymap)
  "Key map for audio controls from headphones")

(redefine-key *top-map* (kbd "XF86AudioMute") '*headphone-map*
              "Headphone controls" "Keymap")

(defvar *bg-map* (make-sparse-keymap)
  "Key map for background controls")

(redefine-key-with-alternatives *root-map* (kbd "B") '*bg-map*
              "Background controls" "Keymap")

(defvar *open-map* (make-sparse-keymap)
  "Key map for quick opening of files.")

(redefine-key-with-alternatives *root-map* (kbd "o") '*open-map*
               "Quick Open" "Keymap")

;;; Help

(redefine-key-with-alternatives *layout-map*
                                (kbd "h")
                                "show-keymap *layout-map*"
                                "Help"
                                "System")
(redefine-key-with-alternatives *root-map*
                                (kbd "h")
                                "show-keymap *root-map*"
                                "Help" "System")
(redefine-key-with-alternatives *top-map*
                                (kbd "H-h")
                                "show-keymap *top-map*"
                                "Help"
                                "System")
(redefine-key-with-alternatives *headphone-map*
                                (kbd "h")
                                "show-keymap *headphone-map*"
                                "Help"
                                "System")
(redefine-key-with-alternatives *bg-map*
                                (kbd "h")
                                "show-keymap *bg-map*"
                                "Help"
                                "System")

(redefine-key-with-alternatives *open-map*
                                (kbd "h")
                                "show-keymap *open-map*"
                                "Help"
                                "System")

;;; Common commands

(redefine-key *top-map* (kbd "H-RET") "exec st" "Terminal" "App")
(redefine-key *root-map* (kbd "RET") "exec" "Run command" "System")
(redefine-key *root-map*
              (kbd "S-RET")
              "exec-in-terminal"
              "Run command in terminal."
              "System")
(redefine-key-with-alternatives *top-map*
                                (kbd "H-w")
                                "exec firefox"
                                "Firefox"
                                "App")

(redefine-key-with-alternatives *top-map*
                                (kbd "H-e")
                                "exec emacsvi"
                                "Emacs"
                                "App")
(redefine-key-with-alternatives *top-map*
                               (kbd "H-m")
                               (kal/execterm "ncmpcpp")
                               "Music player"
                               "App")
(redefine-key-with-alternatives *top-map*
                                (kbd "H-n")
                                (kal/execterm "newsboat")
                                "RSS Feeds"
                                "App")
(redefine-key-with-alternatives *top-map*
                                (kbd "H-p")
                                "exec mpris-control toggle"
                                "Toggle play/pause"
                                "Music controls")
(redefine-key *top-map* (kbd "H-.") "exec mpris-control next"
              "Next song" "Music controls")
(redefine-key *top-map* (kbd "H-,") "exec mpris-control previous"
              "Prev song" "Music controls")
(redefine-key-with-alternatives *root-map*
                                (kbd "p")
                                "exec mpris-choose"
                                "Select active player"
                                "Music controls")
(redefine-key-with-alternatives *top-map*
                                (kbd "H-r")
                                (kal/execterm "lfrun")
                                "File manager"
                                "App")

(redefine-key *top-map*
              (kbd "XF86AudioNext")
              "exec mpris-control next"
              "Next song"
              "Music controls")

(redefine-key *top-map*
              (kbd "XF86AudioPrev")
              "exec mpris-control previous"
              "Prev song"
              "Music controls")

(redefine-key *top-map*
              (kbd "XF86AudioPlay")
              "exec mpris-control play"
              "Play song"
              "Music controls")

(redefine-key *top-map*
              (kbd "XF86AudioStop")
              "exec mpris-control pause"
              "Pause song"
              "Music controls")

(redefine-key *top-map*
              (kbd "XF86AudioPause")
              "exec mpris-control pause"
              "Pause song"
              "Music controls")


(redefine-key-with-alternatives *top-map*
                                (kbd "H-f")
                                "fullscreen"
                                "Fullscreen"
                                "Frame")


;; Audio related controls
(redefine-key *top-map* (kbd "H-=") "pulse-volume-increase"
              "Volume +2" "System")
(redefine-key *top-map* (kbd "H--") "pulse-volume-decrease"
              "Volume -2" "System")
(redefine-key-with-alternatives *top-map* (kbd "H-M") "pulse-toggle-mute"
                                "Toggle mute" "System")
(redefine-key *top-map* (kbd "XF86AudioRaiseVolume")
              "pulse-volume-increase"
              "Volume +2" "System")
(redefine-key *top-map* (kbd "XF86AudioLowerVolume")
              "pulse-volume-decrease"
              "Volume -2" "System")

;; Frame controls
(redefine-key *top-map* (kbd "H-SPC") "pull-hidden-next"
              "Next window on stack" "Frame")
(redefine-key *top-map* (kbd "H-S-SPC") "vpull-hidden-previous"
              "Prev window on stack" "Frame")

(redefine-key-with-alternatives *root-map* (kbd "H-a") "exec rofi -show-icons -show window -columns 3 -modi window"
                                "Window Switcher"
                                "Frame")


(dolist (dir *directions*)
  (kal/register-frame-move-key dir "focus" *top-map* "H-")
  (kal/register-frame-move-key dir "window" *root-map* ""))

(redefine-key-with-alternatives *root-map*
                                (kbd "f")
                                "float-this"
                                "Float window"
                                "Floating")
(redefine-key-with-alternatives *root-map*
                                (kbd "F")
                                "unfloat-this"
                                "Tile window"
                                "Floating")
(redefine-key-with-alternatives *root-map* (kbd "H-f") "flatten-floats"
              "Tile all windows" "Floating")


;; Use the default Emacs keybindings for frames
(redefine-key *root-map* (kbd "1") "only" "Remove all other frames" "Frame")
(redefine-key *root-map* (kbd "2") "vsplit" "Split vertically" "Frame")
(redefine-key *root-map* (kbd "3") "hsplit" "Split horizontally" "Frame")
(redefine-key *root-map* (kbd "0") "remove" "Remove current frame" "Frame")

;;; Uncommon commands

(redefine-key-with-alternatives *root-map* (kbd "b") "kal/toggle-mode-line"
                                "Toggle mode-line"
                                "System")
(redefine-key-with-alternatives *root-map* (kbd "t") "exec torwrap"
                                "Torrents"
                                "App")
(redefine-key-with-alternatives *root-map* (kbd "s") "exec rofi-mpris"
                                "Current song"
                                "Music controls")
(redefine-key-with-alternatives *root-map* (kbd "S") "exec select-from-playlist"
                                "Select from playlist"
                                "Music controls")
(redefine-key-with-alternatives *root-map* (kbd "T") (kal/execterm "htop")
                                "System monitor"
                                "App")
(redefine-key-with-alternatives *root-map* (kbd "H-s")
                                "exec maim -i \"$(xdotool getactivewindow)\" pic-window-\"$(date '+%y%m%d-%H%M-%S').png\""
                                "Screenshot window"
                                "System")

(redefine-key-with-alternatives *root-map* (kbd "w") "windows %f%n %t^]"
              "Show windows"
              "Frame")

(redefine-key-with-alternatives *root-map* (kbd "G") "exec rofi -show window -show-icons"
                                "Show Groups"
                                "System")

(redefine-key *root-map* (kbd "R") "loadrc" "Reload config file." "System")


(redefine-key *root-map* (kbd "P")
              "watch-link-from-clipboard"
              "Open link in clipboard in mpv."
              "Media")

(redefine-key-with-alternatives *open-map* (kbd "p")
                                "exec open-pdf"
                                "Open PDF"
                                "Media")

(redefine-key-with-alternatives *open-map* (kbd "b")
                                "exec list-books"
                                "Open Books"
                                "Media")

;; Headphone controls
(redefine-key *headphone-map* (kbd "XF86AudioRaiseVolume") "exec mpris-control next"
              "Next song" "Music controls")
(redefine-key *headphone-map* (kbd "XF86AudioLowerVolume") "exec mpris-control previous"
              "Prev song" "Music controls")
(redefine-key *headphone-map* (kbd "XF86AudioMute") "pulse-toggle-mute"
              "Mute/unmute" "System")
(redefine-key *headphone-map* (kbd "H-p") "exec mpris-control toggle"
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
