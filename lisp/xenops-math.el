;;; xenops-math.el --- Functions for working with elements of type 'block-math, 'inline-math, and 'table  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Terminology
;;
;; | math element   | either an inline element or a block element    |
;; | inline element | inline math delimited by $...$                 |
;; | block element  | e.g. a \begin{align}...\end{align} environment |

;;; Code:

(require 'xenops-math-latex)

(declare-function xenops-render-at-point "xenops")
(declare-function xenops-render-if-cached "xenops")
(declare-function xenops-apply-operations "xenops-apply")
(declare-function xenops-avy-do-at-element "xenops-avy")
(declare-function xenops-elements-get "xenops-elements")
(declare-function xenops-elements-get-for-types "xenops-elements")
(declare-function xenops-element-copy "xenops-element")
(declare-function xenops-element-get-image "xenops-element")
(declare-function xenops-element-overlays-delete "xenops-element")
(declare-function xenops-element-overlay-get "xenops-element")
(declare-function xenops-overlay-create "xenops-overlay")
(declare-function xenops-parse-element-at-point "xenops-parse")
(declare-function xenops-parse-element-at-point-matching-delimiters "xenops-parse")
(declare-function xenops-util-first-result "xenops-util")
(declare-function xenops-util-goto-line "xenops-util")
(declare-function xenops-util-highlight-current-line "xenops-util")
(declare-function xenops-util-svg-resize "xenops-util")


(defvar xenops-math-image-change-size-factor 1.1
  "The multiplicative factor used when resizing images.

This is the factor by which the image's size will be changed
  under `xenops-math-image-increase-size' and
  `xenops-math-image-decrease-size'.")

(defvar xenops-math-image-current-scale-factor 1.0
  "The current size of images, as a multiple of their default size.

This is the net scale factor resulting from multiple applications
  of `xenops-math-image-increase-size' and
  `xenops-math-image-decrease-size'.")

(defvar xenops-math-image-scale-factor 1.0
  "Scaling factor for SVG math images.

This determines the size of the image in the image file that is
  cached on disk.")

(defvar xenops-math-image-margin 20
  "Number of pixels to be used as left margin for non-inline math images.")

(defvar xenops-math-dollar-delimited-inline-math-delimiters
  '("\\$"
    "\\$"))

(defvar xenops-math-paren-delimited-inline-math-delimiters
  '("\\\\("
    "\\\\)"))

(defvar xenops-math-square-bracket-delimited-inline-math-delimiters
  '("\\\\\\["
    "\\\\\\]"))

(defvar xenops-math-tikz-inline-math-delimiters
  '("\\\\tikz"
    ";"))

(defvar xenops-math-environment-delimited-inline-math-delimiters
  '("\\\\begin{\\(align\\|equation\\|tikzpicture\\|gather\\)\\*?}"
    "\\\\end{\\(align\\|equation\\|tikzpicture\\|gather\\)\\*?}"))

;; Silence compiler: defined elsewhere
(defvar xenops-cache-directory)
(defvar xenops-mode-map)
(defvar xenops-rendered-element-keymap)
(defvar xenops-reveal-on-entry)
(defvar xenops-apply-user-point)

(defun xenops-math-font-lock-keywords ()
  "Create font-lock entry for math elements."
  `((,(xenops-math-block-delimiter-lines-regexp)
     (0
      (xenops-math-block-math-font-lock-handler)))))

(defun xenops-math-activate ()
  "Perform xenops-math responsibilities during minor mode activation."
  (setq-local xenops-math-latex-tasks-semaphore
              (aio-sem xenops-math-latex-max-tasks-in-flight))
  (make-directory xenops-cache-directory t)
  (setq mouse-drag-and-drop-region t)
  (advice-add #'mouse-drag-region :around #'xenops-math-mouse-drag-region-around-advice)
  (advice-add fill-paragraph-function :after #'xenops-math-fill-paragraph-after-advice)
  (advice-add #'TeX-insert-dollar :after #'xenops-math-look-back-and-render-inline-math)
  (define-key xenops-mode-map ")" #'xenops-math-insert-closing-paren)
  (font-lock-add-keywords nil (xenops-math-font-lock-keywords))
  (cursor-sensor-mode +1)
  (add-hook 'before-save-hook #'xenops-render-at-point nil t)
  (add-to-list 'fill-nobreak-predicate #'xenops-math-parse-inline-element-at-point))

(defun xenops-math-deactivate ()
  "Perform xenops-math responsibilities during minor mode deactivation."
  (advice-remove #'mouse-drag-and-drop-region #'xenops-math-mouse-drag-region-around-advice)
  (advice-remove fill-paragraph-function #'xenops-math-fill-paragraph-after-advice)
  (advice-remove #'TeX-insert-dollar #'xenops-math-look-back-and-render-inline-math)
  (cursor-sensor-mode -1)
  (remove-hook 'before-save-hook #'xenops-render-at-point t)
  (font-lock-remove-keywords nil (xenops-math-font-lock-keywords)))

(defun xenops-math-render (element &optional cached-only)
  "Render math element ELEMENT (asynchronously).

If the element is cached, then display the image synchronously.
Otherwise, if CACHED-ONLY is non-nil, schedule an asynchronous
task that will run the necessary external processes to compile
the LaTeX to SVG, and insert the SVG into the buffer."
  (unless (or (xenops-element-get-image element)
              (xenops-element-overlay-get element 'xenops-math-waiting)
              (string-equal "" (s-trim (buffer-substring (plist-get element :begin-content)
                                                         (plist-get element :end-content)))))
    (let ((latex (buffer-substring-no-properties (plist-get element :begin)
                                                 (plist-get element :end))))
      (let* ((colors (xenops-math-latex-get-colors))
             (cache-file (xenops-math-compute-file-name latex colors))
             (cache-file-exists? (file-exists-p cache-file))
             (display-image
              (lambda (element &optional commands)
                (xenops-math-display-image element commands latex cache-file))))
        (cond
         (cache-file-exists?
          (funcall display-image element))
         ((not cached-only)
          (xenops-math-display-waiting element)
          (xenops-math-latex-create-image element latex colors cache-file display-image)))))))

(defun xenops-math-render-below-maybe (element)
  "Render ELEMENT below ELEMENT, so that the rendering can be monitored while editing."
  ;; TODO: This needs to be refactored. The decision of whether to render below or in-place is made
  ;; by xenops-math-display-image. As things stand the name of the current function is inaccurate.
  (unless (eq 'inline-math (plist-get element :type))
    (xenops-math-render element)))

(defun xenops-math-regenerate (element)
  "Regenerate math element ELEMENT.

This is equivalent to deleting any cached image that may exist
and then calling `xenops-render'."
  (let ((cache-file (xenops-math-get-cache-file element)))
    (when cache-file
      (delete-file cache-file)
      (clear-image-cache cache-file))
    (xenops-element-overlays-delete element)
    (xenops-math-render element)))

(defun xenops-math-reveal (element)
  "Remove image overlay for ELEMENT.

If a prefix argument is in effect, also delete its cache file."
  (xenops-element-overlays-delete element)
  (if current-prefix-arg
      (delete-file (xenops-math-get-cache-file element)))
  ;; TODO: is :begin-content for block math off by one?
  (let ((element-type (plist-get element :type))
        (begin-content (plist-get element :begin-content)))
    (goto-char (if (eq element-type 'block-math)
                   (1+ begin-content)
                 begin-content)))
  (xenops-math-render-below-maybe element))

(defun xenops-math-display-waiting (element)
  "Style a math element ELEMENT as waiting.

The style indicates that its processing task is waiting in the
queue to be executed."
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :end))
         (ov (xenops-overlay-create beg end)))
    (overlay-put ov 'face `(:background ,(if (eq (frame-parameter nil 'background-mode) 'light)
                                             "OldLace" "#362b2b")))
    (overlay-put ov 'xenops-overlay-type 'xenops-math-waiting)
    (overlay-put ov 'help-echo "Image-generation task in-progress. \
Use `M-x xenops-cancel-waiting-tasks` to make this element editable.") ov))

(defun xenops-math-display-image (element commands help-echo cache-file)
  "Display SVG image resulting from successful LaTeX compilation of ELEMENT.

COMMANDS are the latex processing commands used to render the
element. HELP-ECHO is the tooltip text to display. CACHE-FILE is
the image cache file."
  (xenops-element-overlays-delete element)
  (let* ((inline-p (eq 'inline-math (plist-get element :type)))
         (margin (if inline-p 0 `(,xenops-math-image-margin . 0)))
         (display-after-element-p (and (not inline-p)
                                       (<= (plist-get element :begin)
                                           (or xenops-apply-user-point (point))
                                           (plist-get element :end))))
         (ov-beg (if display-after-element-p
                     (save-excursion (goto-char (plist-get element :end))
                                     (point-at-bol))
                   (plist-get element :begin)))
         (ov-end (plist-get element :end))
         (ov (xenops-math-make-overlay ov-beg ov-end commands help-echo)))
    (overlay-put ov 'display
                 `(image :type ,(intern  (xenops-math-latex-process-get :image-output-type))
                         :file ,cache-file :ascent center :margin ,margin)))
  (unless (equal xenops-math-image-current-scale-factor 1.0)
    (xenops-math-image-change-size element xenops-math-image-current-scale-factor)))

(defun xenops-math-display-error-badge (element error display-error-p)
  "Style ELEMENT to indicate ERROR during execution of its processing task.

If DISPLAY-ERROR-P is non-nil, then also display the error
immediately (as if the user had selected \"View failing command
output\" from the contextual menu).

Make error details available via hover-over text and contextual
menu."
  (xenops-element-overlays-delete element)
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :begin-content))
         (ov (xenops-overlay-create beg end))
         (keymap (overlay-get ov 'keymap))
         (error-badge "⚠️")
         help-echo)
    (-if-let* ((error-data (plist-get (cdr error) :xenops-aio-subprocess-error-data)))
        (cl-destructuring-bind (failing-command failure-description output) error-data
          (let* ((xenops-math-image-overlay-menu
                  (lambda (event)
                    (interactive "e")
                    (popup-menu
                     `("Xenops"
                       ["View failing command output" (xenops-math-display-process-error-windows
                                                       ,failing-command ,output)]
                       ["Copy failing command" (kill-new ,failing-command)]))
                    event)))
            (setq help-echo (format "External running external process: %s
Right-click on the warning badge to copy the failing command or view its output.

%s"
                                    failure-description
                                    failing-command))
            (define-key keymap [mouse-3] xenops-math-image-overlay-menu))
          (when display-error-p
            (xenops-math-display-process-error-windows failing-command output)))
      (setq help-echo (format "Error processing LaTeX fragment:\n\n%s"
                              (s-join "\n\n" (--map (format "%S" it) error)))))
    (add-text-properties 0 (length error-badge)
                         `(help-echo ,help-echo keymap ,keymap)
                         error-badge)
    (overlay-put ov 'after-string error-badge)
    (overlay-put ov 'help-echo help-echo)))

(defun xenops-math-make-overlay (beg end commands help-echo)
  "Return overlay used to display image of math content.

BEG and END are the overlay extent. COMMANDS are the latex
processing commands used to render the element. HELP-ECHO is the
tooltip text to display."
  (let* ((ov (xenops-overlay-create beg end))
         (keymap (overlay-get ov 'keymap))
         (xenops-math-image-overlay-menu
          (lambda (event)
            (interactive "e")
            (popup-menu
             `("Xenops"
               ["Edit" (progn (goto-char ,beg) (xenops-reveal-at-point))]
               ["Copy LaTeX command" (xenops-math-copy-latex-command ,ov)]))
            event)))
    (overlay-put ov 'help-echo help-echo)
    (overlay-put ov 'commands commands)
    (set-keymap-parent keymap xenops-rendered-element-keymap)
    (define-key keymap [mouse-3] xenops-math-image-overlay-menu)
    ov))

(defun xenops-math-display-process-error-windows (command output)
  "Display windows containing information about an error in an external process.

COMMAND is the command used to start process, and OUTPUT is its standard output."
  ;; HACK, TODO: should make the input file available in the error-data object constructed by
  ;; `xenops-aio-subprocess', rather than inferring it from the full command.
  (let ((input-file (car (last (s-split " " command)))))
    (let* ((input-buf (xenops-math-get-process-input-buffer input-file))
           (output-buf (xenops-math-get-process-output-buffer output))
           (first-error-line
            (with-current-buffer output-buf
              (when (re-search-forward "^!" nil t)
                (xenops-util-highlight-current-line)
                (save-excursion
                  (and (re-search-forward "^l\.\\([0-9]+\\)" nil t)
                       (string-to-number (match-string 1))))))))
      (when first-error-line
        (with-current-buffer input-buf
          (xenops-util-goto-line first-error-line)
          (xenops-util-highlight-current-line)))
      (-when-let* ((output-win (display-buffer output-buf)))
        (with-selected-window output-win
          (-when-let* ((input-win (display-buffer input-buf '(display-buffer-below-selected))))
            (with-selected-window input-win (recenter-top-bottom)))
          (recenter-top-bottom))))))

(defun xenops-math-get-process-input-buffer (input-file)
  "Return a buffer containing the input for an external process.

INPUT-FILE is a file containing the input."
  (let ((buf (get-buffer-create "*Xenops external command input*")))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert-file-contents input-file)
        (-when-let* ((mode-fn (assoc-default input-file auto-mode-alist 'string-match)))
          (funcall mode-fn)))
      (display-line-numbers-mode)
      buf)))

(defun xenops-math-get-process-output-buffer (output)
  "Return a buffer containing external process output OUTPUT."
  ;; TODO: is (TeX-error-overview-mode) useful?
  (let ((buf (get-buffer-create "*Xenops external command output*")))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert output))
      (goto-char (point-min)))
    buf))

(defun xenops-math-copy-latex-command (overlay)
  "Copy external latex command to clipboard (kill-ring).

OVERLAY is an element overlay from which the commands can be obtained."
  (let ((latex-command (car (overlay-get overlay 'commands))))
    (kill-new (s-join " " latex-command))))

(defun xenops-math-set-marker-on-element (element)
  "Create a marker pointing at the current :begin position of ELEMENT."
  (plist-put element :begin-marker (set-marker (make-marker) (plist-get element :begin))))

(defun xenops-math-deactivate-marker-on-element (element)
  "Deactivate the marker for ELEMENT created by `xenops-math-set-marker-on-element'."
  (if-let* ((marker (plist-get element :begin-marker)))
      (set-marker marker nil)))

(defun xenops-math-image-increase-size (element)
  "Increase ELEMENT image size."
  (xenops-math-image-change-size element xenops-math-image-change-size-factor))

(defun xenops-math-image-decrease-size (element)
  "Decrease ELEMENT image size."
  (xenops-math-image-change-size element (/ 1 xenops-math-image-change-size-factor)))

(defun xenops-math-image-change-size (element factor)
  "Change ELEMENT image size by multiplicative FACTOR."
  (-if-let* ((image (xenops-element-get-image element)))
      (when (eq (image-property image :type) 'svg)
        (image-flush image)
        (let* ((data (or (eval (image-property image :data))
                         (and (f-exists? (image-property image :file))
                              (prog1 (f-read-text (image-property image :file))
                                (setf (image-property image :file) nil))))))
          (if data
              (setf (image-property image :data) (xenops-util-svg-resize data factor)))))))

(defun xenops-math-block-delimiter-lines-regexp ()
  "A regexp matching the start or end line of any block math element."
  (format "\\(%s\\)"
          (s-join "\\|"
                  (apply #'append (xenops-elements-get-for-types '(block-math table) :delimiters)))))

(defun xenops-math-block-math-font-lock-handler ()
  "Font-lock handler for math elements."
  (add-face-text-property (match-beginning 0) (match-end 0) 'fixed-pitch)
  (xenops-math-add-cursor-sensor-property)
  nil)

(defun xenops-math-inline-math-font-lock-handler ()
  "Font-lock handler for inline math elements."
  (xenops-math-add-cursor-sensor-property)
  nil)

(defun xenops-math-add-cursor-sensor-property ()
  "Render a math element whenever the cursor leaves the element.

Suppose we have inline element 1$345$7 where the integers are the
buffer positions of the corresponding characters. The following
tables shows required behavior for cursor position transitions.

| old pos | new pos | behavior      | notes                                   |
|---------+---------+---------------+-----------------------------------------+
|       4 |       3 | do not render | 3 is pos for inserting at element start |
|       3 |       1 | render        |                                         |
|       5 |       6 | do not render | 6 is pos for inserting at element end   |
|       5 |       7 | render        |                                         |
|       5 |       6 | do not render |                                         |

The above is achieved by setting the `cursor-sensor-functions'
property on positions 3-6 inclusive (which are 1+:begin and :end indices).

In addition, we require the following text property inheritance
behavior on insertion:

| pos | behavior                  | implementation                           |
|-----+---------------------------+------------------------------------------|
|   2 | do not inherit from right | front-nonsticky: default Emacs behaviour |
| 3-6 | inherit from left         | rear sticky: default Emacs behaviour     |
|   7 | do not inherit from left  | set rear-nonsticky on 6                  |"
  (-when-let* ((element (xenops-math-parse-element-at-point)))
    (let ((beg (1+ (plist-get element :begin)))
          (end (plist-get element :end))
          (props '(cursor-sensor-functions (xenops-math-handle-element-transgression))))
      (add-text-properties beg end props)
      (add-text-properties (1- end) end '(rear-nonsticky (cursor-sensor-functions))))))

(defun xenops-math-handle-paste ()
  "If the text to be pasted is a math element then handle the paste."
  (let* ((copied-text (current-kill 0 'do-not-rotate)))
    (-when-let* ((copied-element (xenops-math-parse-element-from-string copied-text)))
      (-if-let* ((element-at-point (xenops-math-parse-element-at-point)))
          (xenops-math-handle-paste-in-math-element copied-text copied-element element-at-point)
        (let* ((copied-element-is-block (not (eq 'inline-math (plist-get copied-element :type))))
               (copied-text
                (s-trim
                 (if current-prefix-arg
                     (string-remove-prefix "\n" (read-from-minibuffer "Text to paste: " copied-text))
                   copied-text))))
          ;; (when (and copied-element-is-block
          ;;            (save-excursion (goto-char (point-at-bol)) (looking-at-p "[ \t]*$")))
          ;;   (goto-char (point-at-bol)))
          (save-excursion
            (progn
              (insert-for-yank copied-text)
              (rotate-yank-pointer 1))))
        (xenops-math-render (xenops-math-parse-element-at-point))
        (length copied-text)))))

(defun xenops-math-handle-paste-in-math-element (copied-text copied-element element-at-point)
  "Paste COPIED-TEXT. Point is inside math element ELEMENT-AT-POINT.

Since we are in a math element, we strip the delimiters from the copied text before pasting."
  (let* ((copied-element-is-block (not (eq 'inline-math (plist-get copied-element :type))))
         (element-at-point-is-block (not (eq 'inline-math (plist-get element-at-point :type))))
         (copied-text
          (substring copied-text
                     ;; `xenops-math-parse-element-from-string' returns 1-based indexes,
                     ;; suitable for indexing into a buffer; string is 0-based.
                     (1- (plist-get copied-element :begin-content))
                     (1- (plist-get copied-element :end-content))))
         (copied-text
          (s-trim
           (if (or current-prefix-arg copied-element-is-block)
               (string-remove-prefix "\n" (read-from-minibuffer "Text to paste: " copied-text))
             copied-text))))
    (when (and copied-element-is-block
               element-at-point-is-block
               (save-excursion (goto-char (point-at-bol)) (looking-at-p "[ \t]*$")))
      (goto-char (point-at-bol)))
    (progn
      (insert-for-yank copied-text)
      (rotate-yank-pointer 1))))

(defun xenops-math-paste ()
  "Paste handler for math elements."
  (or (xenops-math-handle-paste) (yank)))

(defun xenops-math-look-back-and-render-inline-math (&rest _)
  "Render an inline math element when a closing delimiter is inserted."
  ;; Hack:
  ;;
  ;; Unless `TeX-electric-math' is set to '("$" . "$") then, without the following, an inline
  ;; math element will not be rendered when a closing dollar is inserted.
  ;;
  ;; Similarly, unless `LaTeX-electric-left-right-brace' is t, or `TeX-electric-math' is
  ;; '("\\(" . "\\)") then, without the following the inline element will not be rendered when a
  ;; closing "\)" is entered.
  ;;
  ;; This code executes on every insert! Hard-coding the delimiters, instead of
  ;; (let ((closing-delimiters
  ;;         (apply #'append (mapcar #'cdr (xenops-elements-get 'inline-math :delimiters)))))
  (if (or (looking-back (cadr xenops-math-dollar-delimited-inline-math-delimiters)
                        (- (point) 1))
          (looking-back (cadr xenops-math-paren-delimited-inline-math-delimiters)
                        (- (point) 2)))
      (save-excursion
        (goto-char (match-beginning 0))
        (if-let* ((element (xenops-math-parse-inline-element-at-point)))
            (xenops-math-render element)))))

(defun xenops-math-insert-closing-paren ()
  "Insert ). If this closed an inline math element then render it."
  (interactive)
  (insert ")")
  (xenops-math-look-back-and-render-inline-math))

(defun xenops-math-fill-paragraph-after-advice (&rest _)
  "Re-render cached images after `fill-paragraph'."
  (let ((forward-paragraph-fn (if (fboundp 'LaTeX-forward-paragraph)
                                  'LaTeX-forward-paragraph
                                'forward-paragraph))
        (backward-paragraph-fn (if (fboundp 'LaTeX-backward-paragraph)
                                   'LaTeX-backward-paragraph
                                 'backward-paragraph)))
    (save-excursion
      ;; If point is at the start of a paragraph, LaTeX-fill-paragraph fills the paragraph
      ;; ahead. Therefore we move to the end before going back to locate the beginning.
      (funcall forward-paragraph-fn)
      (push-mark (point) t t)
      (funcall backward-paragraph-fn)
      (if (region-active-p)
          (xenops-render-if-cached)))))

(defun xenops-math-parse-element-from-string (element-string)
  "Parse a math element from ELEMENT-STRING."
  (with-temp-buffer
    (save-excursion (insert element-string))
    (-if-let* ((element (xenops-math-parse-element-at-point)))
        (when (eq (- (plist-get element :end)
                     (plist-get element :begin))
                  (length element-string))
          element))))

(defun xenops-math-handle-element-transgression (_window oldpos event-type)
  "Render a math element when point leaves it.

WINDOW is currently ignored. OLDPOS is the previous location of
point. EVENT-TYPE is the type of cursor sensor event that
triggered this handler."
  (cond ((eq event-type 'left)
         ;; TODO: The following check shouldn't be necessary, but in practice the 'left event is
         ;; being triggered sometimes when point is still in a math/table element. The double-parse
         ;; check causes us to render iff we are not strictly *inside* an element, i.e. we do
         ;; render if we are immediately before or immediately after (as will be the case if point
         ;; has been moved with left/right arrow keys or C-f/C-b).
         (unless (and (xenops-math-parse-element-at-point)
                      (xenops-math-parse-element-at (1- (point))))
           (let ((was-in (xenops-math-parse-element-at oldpos)))
             (and was-in
                  (not (xenops-element-get-image was-in))
                  (xenops-math-render was-in)))))
        ((and xenops-reveal-on-entry (eq event-type 'entered))
         (-when-let* ((element (xenops-math-parse-element-at-point)))
           (let ((entered-from-right (= oldpos (1+ (point)))))
             (xenops-math-reveal element)
             (when entered-from-right
               (goto-char (plist-get element :end-content))))))))

(defun xenops-math-mouse-drag-region-around-advice (mouse-drag-region-fn start-event)
  "If point is in a math element, then make mouse drag the associated image.

1. Select the math element as the currently active region.
2. Temporarily alter `tooltip-show' so that it displays the image.

MOUSE-DRAG-REGION-FN is the function being advised. START-EVENT
is the start event of the mouse drag."
  (-if-let* ((element (xenops-math-parse-element-at (posn-point (event-start start-event)))))
      (progn
        (push-mark (plist-get element :begin))
        (goto-char (plist-get element :end))
        (let ((tooltip-show-fn (symbol-function 'tooltip-show))
              (image-tooltip (propertize " "
                                         'display
                                         ;; TODO: the file path should be stored somewhere, not recomputed.
                                         ;; TODO: detect image type
                                         `(image . (:file ,(xenops-math-get-cache-file element) :type svg)))))
          (cl-letf (((symbol-function 'mouse-posn-property)
                     (lambda (&rest _) 'region))
                    ((symbol-function 'tooltip-show)
                     (lambda (_ &rest args)
                       (apply tooltip-show-fn image-tooltip args))))
            (funcall mouse-drag-region-fn start-event))))
    (funcall mouse-drag-region-fn start-event)))

(defun xenops-math-parse-element-at (pos)
  "Parse math element at buffer position POS."
  (save-excursion
    (goto-char pos)
    (xenops-math-parse-element-at-point)))

(defun xenops-math-parse-element-at-point ()
  "Parse any math element at point."
  (or (xenops-math-parse-inline-element-at-point)
      (xenops-math-parse-block-element-at-point)
      (xenops-math-parse-table-at-point)))

(defun xenops-math-parse-block-element-at-point ()
  "Parse block math element at point."
  (xenops-parse-element-at-point 'block-math))

(defun xenops-math-parse-table-at-point ()
  "Parse table element at point."
  (xenops-parse-element-at-point 'table))

(defun xenops-math-parse-inline-element-at-point ()
  "Parse any inline math element at point."
  (or (xenops-math-parse-homo-delimited-inline-element-at-point
       (car xenops-math-dollar-delimited-inline-math-delimiters))
      (xenops-util-first-result
       #'xenops-math-parse-hetero-delimited-inline-element-at-point
       (list xenops-math-paren-delimited-inline-math-delimiters
             xenops-math-square-bracket-delimited-inline-math-delimiters
             xenops-math-tikz-inline-math-delimiters
             xenops-math-environment-delimited-inline-math-delimiters))))

(defun xenops-math-parse-hetero-delimited-inline-element-at-point (delimiters)
  "Parse inline math element at point for which start and end delimiters differ.

DELIMITERS is the delimiter pair sought."
  (xenops-parse-element-at-point 'inline-math (point-at-bol) (point-at-eol) delimiters))

(defun xenops-math-parse-homo-delimited-inline-element-at-point (delimiter)
  "Parse an inline math element at point for which both the start and end delimiter are DELIMITER."
  ;; This is a bit awkward since the start and end delimiters are the same.
  ;;
  ;; There are 3 relevant editing states:
  ;;
  ;; 1. Point is outside homo-delimited math.
  ;; 2. User has inserted one delimiter and is currently writing homo-delimited math.
  ;; 3. Point is inside homo-delimited math.
  ;;
  ;; These are distinguished by the parity of the number of delimiters to the left and right of
  ;; point:
  ;;
  ;; | left count | right count | editing state |
  ;; |------------+-------------+---------------|
  ;; | Even       | Even        | Outside       |
  ;; | Odd        | Even        | Inserting     |
  ;; | Even       | Odd         | Inserting   |
  ;; | Odd        | Odd         | Inside        |
  (save-excursion
    (and (or (cl-oddp (count-matches delimiter (point-at-bol) (point)))
             ;; We need the parse to succeed when point is before an opening delimiter, since that
             ;; is the behavior of `xenops-parse-element-at-point'.
             (and (looking-at delimiter)
                  (progn (forward-char) t)))
         (cl-oddp (count-matches delimiter (point) (point-at-eol)))
         (xenops-parse-element-at-point-matching-delimiters
          'inline-math
          (list delimiter delimiter)
          (point-at-bol)
          (point-at-eol)))))

(defun xenops-math-concatenate (beg end)
  "Concatenate and re-render contiguous block math elements in region.

BEG and END define the region acted on."
  (interactive "r")
  (let* ((boundary-regexp
          (format "\\(\n?%s\n?\\)"
                  (s-join "\\|"
                          (cl-loop
                           for pair in (xenops-elements-get 'block-math :delimiters)
                           collecting (s-join "[ \t\n]+" (--map (s-chop-prefix "^" it) (reverse pair)))))))
         (concatenated?))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward boundary-regexp end t)
        (setq concatenated? t)
        (replace-match " \\\\\\\\\n")))
    (when concatenated?
      (push-mark)
      (save-excursion
        (goto-char beg)
        (xenops-apply-operations '(render)))
      (pop-mark))))

(defun xenops-math-copy-and-paste-element ()
  "Copy and paste a math element using avy."
  (let ((avy-action
         (lambda (pt)
           (let ((element))
             (save-excursion
               (goto-char
                ;; TODO: hack: This should be just `pt`, but inline
                ;; math elements are not recognized when point is on
                ;; match for first delimiter.
                (1+ pt))
               (setq element (xenops-math-parse-element-at-point))
               (when element (xenops-element-copy element)))
             (when element
               (save-excursion (xenops-math-paste)))))))
    (xenops-avy-do-at-element '(block-math inline-math))))

(defun xenops-math-get-cache-file (element)
  "Return the name of the SVG image cache file for ELEMENT."
  ;; TODO: the file path should be stored somewhere, not recomputed.
  (let* ((beg (plist-get element :begin))
         (end (plist-get element :end))
         (latex (buffer-substring-no-properties beg end))
         (colors (save-excursion
                   (goto-char beg)
                   (xenops-math-latex-get-colors))))
    (xenops-math-compute-file-name latex colors)))

(defun xenops-math-file-name-static-hash-data ()
  "Return static data used to compute the math content hash."
  (let ((hash-data (list xenops-math-latex-process
                         xenops-math-latex-process-alist
                         org-format-latex-header
                         org-format-latex-options)))
    (if (eq major-mode 'org-mode)
        (append hash-data (list org-latex-default-packages-alist
                                org-latex-packages-alist))
      hash-data)))

(defun xenops-math-compute-file-name (latex colors)
  "Compute the cache file name for LATEX math content using COLORS."
  (let* ((data (append (xenops-math-file-name-static-hash-data) (list latex colors)))
         (hash (sha1 (prin1-to-string data))))
    (format "%s.%s"
            (f-join (f-expand xenops-cache-directory) hash)
            (xenops-math-latex-process-get :image-output-type))))

(provide 'xenops-math)

;;; xenops-math.el ends here
