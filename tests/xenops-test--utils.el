;; -*- lexical-binding: t -*-

(defmacro xenops-test--with-xenops-render (buffer-contents &rest body)
  `(let ((xenops-cache-directory (make-temp-file "xenops-test-" 'dir))
         ;; We are relying on this file being treated as its own master file by
         ;; `TeX-region-create'. If the file name does not end in .tex, then a master file will be
         ;; sought with the .tex suffix, and this will fail.
         (file (make-temp-file "xenops-test-" nil ".tex"))
         (before "\\documentclass{article}\n\\begin{document}\n")
         (after "\n\\end{document}"))
     (with-temp-buffer
       (insert (concat before ,buffer-contents after))
       (write-file file)
       (LaTeX-mode)
       (xenops-mode)
       (mark-whole-buffer)
       (xenops-render)
       (goto-char (length before))
       ,@body)))


(defun xenops-test--do-apply-parse-next-element-test (input-text &rest expected-properties)
  (with-temp-buffer
    (save-excursion (insert input-text))
    (let ((element (xenops-apply-parse-next-element)))
      (cl-loop for (k v) in (-partition 2 expected-properties)
               do (should (equal (plist-get element k) v))))))

