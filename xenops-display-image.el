(defvar xenops-display-image-width 512)

(defvar xenops-display-image-pngpaste-executable "pngpaste")

(defvar xenops-display-image-latex-template
  "\\includegraphics[width=400pt]{%s}"
  "LaTeX code for inclusion of a pasted image in the LaTeX
  document. This must be a string of valid LaTeX code containing
  a single %s placeholder, which will be replaced by the image
  file path. Use a double backslash here to produce a single
  backslash in the resulting LaTeX.")

(defun xenops-display-image-activate ()
  (define-key xenops-mode-map [(super v)] 'xenops-display-image-insert-image-from-clipboard))

(defun xenops-display-image- (element)
  (let ((org-element (plist-put element :type "file")))
    (xenops-display-image-- `(link ,org-element) xenops-display-image-width nil nil ".")))

(defun xenops-display-image-hide- (element)
  (interactive)
  ;; TODO: improve
  (save-restriction
    (narrow-to-region (plist-get element :begin)
                      (plist-get element :end))
    (org-remove-inline-images)
    (widen)))

(defun xenops-display-image-insert-image-from-clipboard ()
  (interactive)
  (let ((output-file)
        (temp-file (make-temp-file "xenops-image-from-clipboard-")))
    (with-temp-buffer
      ;; TODO: I think Emacs can do this natively without pngpaste
      ;; See `gui-selection-value'.
      (let ((exit-status (call-process xenops-display-image-pngpaste-executable nil `(:file ,temp-file) nil "-")))
        (if (= exit-status 0)
            (progn
              (setq output-file
                    (read-file-name "Save image as: " (format "%s/" default-directory)))
              (when (file-exists-p output-file) (error "File exists: %s" output-file))
              (copy-file temp-file output-file t)))))
    (if output-file
        (insert (format xenops-display-image-latex-template
                        (file-relative-name output-file)))
      (call-interactively 'yank))))

(defun xenops-display-image-- (link width include-linked refresh file-extension-re)
  ;; TODO: Hack: This is taken from `org-display-inline-images'.
  (when (and (equal "file" (org-element-property :type link))
             (or include-linked
                 (null (org-element-contents link)))
             (string-match-p file-extension-re
                             (org-element-property :path link)))
    (let ((file (expand-file-name
                 (org-link-unescape
                  (org-element-property :path link)))))
      (when (file-exists-p file)
        (let ((old (get-char-property-and-overlay
                    (org-element-property :begin link)
                    'org-image-overlay)))
          (if (and (car-safe old) refresh)
              (image-refresh (overlay-get (cdr old) 'display))
            (let ((image (create-image file
                                       (and width 'imagemagick)
                                       nil
                                       :width width)))
              (when image
                (let ((ov (make-overlay
                           (org-element-property :begin link)
                           (progn
                             (goto-char
                              (org-element-property :end link))
                             (skip-chars-backward " \t")
                             (point)))))
                  (overlay-put ov 'display image)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put
                   ov 'modification-hooks
                   (list 'org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))

(provide 'xenops-display-image)
