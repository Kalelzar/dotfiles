(in-package :stumpwm)

(defmacro define-limited-iteraction-mode (name apps keymap)
  "Create a new toggle-able mode called NAME.
   It acts as a limited version of Stump WM where the user only has access to a limited amount of APPS and `*top-map*' is set to KEYMAP."
  `(defcommand ,name () ()
      (segregate-environment)
      (spawn-according-to-app-entries ,apps)
      (exchange-top-map ,keymap)
      ))

(defun segregate-environment ()
   "Set up the environment for the limited interaction mode.
    Create new groups, switch view to them and so on."
   )


(defstruct app-entry
  (app nil
       :type string
       );:documentation "The name of the APP defined by this entry.")
  (command nil
           :type string
           );:documentation "The COMMAND to be called for starting the app.")
  (props '()
         :type list)

  )

(defun app-entry= (a b)
       (and (and (app-entry-p a)
                 (app-entry-p b))
            (string= (app-entry-command a)
                     (app-entry-command b)
                ))
       )

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
                            :test #'string=
                      ))
    )
  )


(defun spawn-according-to-app-entries (apps)
  (dolist (app apps)
    (when (app-entry-p app)
      (run-or-raise (app-entry-command app) (app-entry-props app))
      ;(move-window-to-group (current-window) (app-entry-group app))
      )
    ))


(defun exchange-top-map (keymap))
