(defvar xenops-display-image-width 512)

(defvar xenops-display-image-regexp
  "[ \t]*\\\\includegraphics\\(\\[[^]]+\\]\\)?{\\([^}]+\\)}")

(defun xenops-display-image-at-point ()
  (interactive)
  (let ((context (xenops-parse-element-at-point)))
    (when (eq (car context) 'link)
      (flet ((org-element-context () context))
        (xenops-display-image context xenops-display-image-width nil nil ".")))))

(defun xenops-hide-image-at-point ()
  (interactive)
  (org-remove-inline-images))

(defun xenops-display-image-parse-image-at-point ()
  (when (save-excursion (beginning-of-line) (looking-at xenops-display-image-regexp))
    `(link (:type "file" :begin ,(match-beginning 0) :end ,(match-end 0) :path ,(expand-file-name (match-string 2))))))

(defun xenops-display-image (link width include-linked refresh file-extension-re)
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
