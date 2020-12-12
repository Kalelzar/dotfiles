(in-package :stumpwm)

(defstruct window-action
  (title ".*" :type string)
  (class ".*" :type string)
  (type  ".*" :type string)
  (role  ".*" :type string)
  (resource ".*" :type string)
  (group nil)
  (frame nil)
  (action nil))


(defun matches-window-action (w a)
  (let* ((title  (window-action-title    a))
         (class  (window-action-class    a))
         (type   (window-action-type     a))
         (role   (window-action-role     a))
         (res    (window-action-resource a))
         (group  (window-action-group    a))
         (frame  (window-action-frame    a))
         (titlep (title-re-p w
                             title))
         (classp (class-re-p w
                             class))
         (rolep  (role-re-p w
                            role ))
         (resp   (res-re-p w
                           res))
         (groupp (or (null group)
                     (grouped-p w
                                group)))
         (framep (or (null frame)
                     (in-frame-p w
                                 frame))))
    (and titlep classp rolep resp groupp framep))


(execute-window-action (make-window-action
                        :class "firefox"
                        :action #'(lambda (w)
                                    (focus-window w)
                                    (send-meta-key (current-screen) (kbd "C-w"))
                                    )))

(send-fake-key (current-window) (kbd "C-d"))

(defun execute-window-action (action)
  (when (window-action-p action)
    (act-on-matching-windows (window)
                             (matches-window-action window
                                                    action)
                             (funcall (window-action-action action)
                                      window))))
