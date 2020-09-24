(in-package :stumpwm)

(defvar limited-interaction-p nil
  "t if a limited interaction mode is active, nil otherwise.")

(defmacro define-limited-iteraction-mode (name apps keymap)
  "Create a new toggle-able mode called NAME.
   It acts as a limited version of Stump WM where the user only has access to a limited amount of APPS and `*top-map*' is set to KEYMAP."
  `(defcommand ,name () ()
      (segregate-environment)
      (spawn-according-to-app-entries ,apps)
      (exchange-top-map ,keymap)
      (setf limited-interaction-p (not limited-interaction-p))
      ))

(defun segregate-environment ()
   "Set up the environment for the limited interaction mode.
    Create new groups, switch view to them and so on."
   (if limited-interaction-p
       (progn
         (cleanup-segregated-environment))
       (progn
            (gnew   "L1")
            (gnewbg "L2")
            (gnewbg "L3"))))

(defun cleanup-segregated-environment ()
  (dolist (i '("1" "2" "3"))
    (gselect (concat "L" i))
    (dolist (w (group-windows (current-group)))
          (delete-window w))
    (gkill)))


(defstruct app-entry
  (app nil :type string)
  (command nil :type string)
  (props '() :type list))

(defun app-entry= (a b)
       (and (and (app-entry-p a)
                 (app-entry-p b))
            (string= (app-entry-command a)
                     (app-entry-command b))))

(defvar *app-entries* nil
  "An alist of app entries")

(setf *app-entries* nil)

(defun define-app-entry (command &key
                                   (app nil)
                                   (props nil))
  (let* ((appname (if app app command))
         (app-entry (make-app-entry :app appname
                                    :command command
                                    :props props)))
    (setf *app-entries*
          (create-or-append appname
                            app-entry
                            *app-entries*
                            :test #'string=))))


(defun get-app-entry (name)
  (car (cdr (assoc name *app-entries* :test #'string= ))))

(defun to-list-of-app-entries (&rest apps)
  (mapcar #'get-app-entry apps))

(defun spawn-according-to-app-entries (apps)
  (dolist (app apps)
    (message (app-entry-command app))
    (when (app-entry-p app)
      (run-or-pull (app-entry-command app)
                    (app-entry-props app)))))

(defvar *true-top-map* *top-map*
  "A place to store the true `*top-map*' when it is overridden by a limited interaction mode.")

(defun store-top-map ()
  (setf *true-top-map* *top-map*))

(defun restore-top-map ()
  (setf *top-map* *true-top-map*))

(defun exchange-top-map (keymap)
  (if limited-interaction-p
      (restore-top-map)
      (progn
        (store-top-map)
        (setf *top-map* keymap))))

(defvar *ltop-map*
  (stumpwm:make-sparse-keymap)
  "A keymap containing the basic keybindings for a limited interaction mode.")


(mapcar #'(lambda (x)
            (redefine-key *ltop-map* (kbd (concat "H-" (write-to-string x)))
                          (format nil "gselect L~D" x)
                          (format nil "Select group L~D" x))
            (suppress-command (format nil "Select group L~D" x))
            (redefine-key *ltop-map* (kbd (concat "H-s-" (write-to-string x)))
                          (format nil "gmove L~D" x)
                          (format nil "Move to group L~D" x))
                    (suppress-command (format nil "Move to group L~D" x)))
        (range 4 :min 1 :step 1))

(redefine-key *ltop-map*
              (kbd "H-h")
              "show-keymap *ltop-map*"
              "Help"
              "System")

(gmove "1")

(define-app-entry
           "emacsvi"
           :app "Emacs"
           :props '())

(define-limited-iteraction-mode
    test-mode
    (to-list-of-app-entries "Emacs")
  *ltop-map*)



(redefine-key *ltop-map*
              (kbd "H-q")
              "test-mode"
              "Exit limited interaction mode."
              "System")
