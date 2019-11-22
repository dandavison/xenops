(defvar xenops-image-width 512)

(defvar xenops-image-directory nil
  "The directory in which Xenops should offer to save images when
pasted from the system clipboard.")

(defvar xenops-image-pngpaste-executable "pngpaste")

(defvar xenops-image-latex-template
  "\\includegraphics[width=400pt]{%s}"
  "LaTeX code for inclusion of a pasted image in the LaTeX
  document. This must be a string of valid LaTeX code containing
  a single %s placeholder, which will be replaced by the image
  file path. Use a double backslash here to produce a single
  backslash in the resulting LaTeX.")

(defun xenops-image-render (element)
  (let ((image (create-image (plist-get element :path)
                             'imagemagick nil :width xenops-image-width)))
    (add-text-properties (plist-get element :begin)
                         (plist-get element :end)
                         `(display ,image keymap ,xenops-rendered-element-keymap))))

(defun xenops-image-reveal (element)
  (remove-text-properties (plist-get element :begin)
                          (plist-get element :end)
                          '(display nil keymap nil)))

(defun xenops-image-parse-at-point ()
  (if (looking-at (caar (xenops-elements-get 'image :delimiters)))
      (xenops-image-parse-match nil)))

(defun xenops-image-parse-match (element)
  ;; A match has just been made for the opening delimiter and element
  ;; is a plist containing data from that match, but needs more data
  ;; to be a properly populated element.
  ;; TODO: This API is inconsistent with xenops-math.
  (append element `(:type image
                          :begin ,(match-beginning 0)
                          :end ,(match-end 0)
                          :path ,(expand-file-name (match-string 2)))))

(defun xenops-image-handle-paste ()
  (interactive)
  (let ((temp-file (make-temp-file "xenops-image-from-clipboard-"))
        (output-file))
    (with-temp-buffer
      ;; TODO: I think Emacs can do this natively without pngpaste
      ;; See `gui-selection-value'.
      (let ((exit-status
             (call-process xenops-image-pngpaste-executable nil `(:file ,temp-file) nil "-")))
        (if (= exit-status 0)
            (let ((file-name-suggestion (xenops-image-get-file-name-suggestion
                                         (substring (sha1 (f-read-bytes temp-file)) 0 4)
                                         "png")))
              (setq output-file
                    (read-file-name "Save image as: "
                                    (or xenops-image-directory default-directory)
                                    nil nil file-name-suggestion))
              (when (file-exists-p output-file) (error "File exists: %s" output-file))
              (copy-file temp-file output-file t)))))
    (when output-file
      (save-excursion
        (insert (format xenops-image-latex-template
                        (file-relative-name output-file))))
      (xenops-image-render (xenops-apply-get-next-element (point-max)))
      t)))

(defun xenops-image-get-file-name-suggestion (identifier extension)
  (save-excursion
    (let ((outline-regexp "\\\\\\(sub\\)*section{\\([^}]*\\)}")
          pos headings)
      (ignore-errors (outline-back-to-heading))
      (setq pos (1+ (point-max)))
      (while (and (< (point) pos) (outline-on-heading-p))
        (setq headings
              (push (s-downcase (s-replace-regexp "[ :/]+" "-" (match-string 2)))
                    headings))
        (setq pos (point))
        (outline-up-heading 1))
      (format "%s--%s--%s.%s"
              (f-base (buffer-file-name))
              (s-join "--" headings)
              identifier
              extension))))

(provide 'xenops-image)
