(require 'f)
(require 'dash)
(require 'lsp-mode)
(require 'cc-cmds)
(require 'cc-mode)


(defun lsp-print-signature ()
  "Print the type signature and documentation of the thing at
point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (lsp:hover-contents))))
    (when (and contents (not (equal contents "")))
      (string-trim-right (lsp--render-on-hover-content contents t)))))

(defun c--describe-block ()
  "Describe current enclosing block in C/C++."
  (save-excursion
    (let ((func (which-function))
        (case-fold-search nil))
      (c-beginning-of-defun)
      (re-search-forward func)
      (backward-char))
    (lsp-print-signature)))

(defun c-describe-block ()
  "Describe current enclosing block in C/C++."
  (interactive)
  (message (c--describe-block)))


(defun c-cycle-files (&optional backward)
  "Cycle through alternatives of this c++ file.
The possible options for files are:
Header, Source, Test Source.
If BACKWARD is non-nil then cycle in reverse."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-filename (f-base current-file))
         (current-dir (f-slash (f-dirname current-file)))
         (current-project (f-slash (f-parent current-dir)))
         (current-project-name (f-slash (f-base current-project)))
         (header-location (f-join current-project
                                  current-project-name
                                  (concat current-filename
                                          ".hpp")))
         (source-location (f-join current-project
                                  current-project-name
                                  (concat current-filename
                                          ".cpp")))
         (test-location (f-join current-project
                                "test"
                                (concat current-filename
                                        ".cpp"))))
    (cond
     ((string= current-file header-location)
      (if (null backward)
          (find-file source-location)
          (find-file test-location)))
     ((string= current-file source-location)
      (if (null backward)
          (find-file test-location)
          (find-file header-location)))
     ((string= current-file test-location)
      (if (null backward)
          (find-file header-location)
          (find-file source-location))))))

(defun c-cycle-files-forward ()
  "Cycle forward through alternatives of this c++ file with `c-cycle-files'."
  (interactive)
  (c-cycle-files))

(define-key c++-mode-map (kbd "M-<up>") 'c-cycle-files-forward)

(defun c-cycle-files-backward ()
  "Cycle backward through alternatives of this c++ file with `c-cycle-files'."
  (interactive)
  (c-cycle-files t))

(define-key c++-mode-map (kbd "M-<down>") 'c-cycle-files-backward)
