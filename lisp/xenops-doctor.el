;;; xenops-doctor.el --- Cure unhappy Xenopsen -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:
(require 's)

(defun xenops-doctor (&optional quiet)
  "Check your Emacs environment for any problems that will affect Xenops.

The checks run are:

- This Emacs version works with Xenops.
- External latex and dvisvgm executables can be located.
- This Emacs is able to display SVGs.
- Emacs is not running as a terminal application.

With optional argument QUIET, don't display any output unless
there are problems."
  (interactive)
  (let ((problems)
        (emacs-major-version (string-to-number (car (s-split "\\." emacs-version))))
        (exec-path-help
         (xenops-doctor-format
          "If that all looks good, but I am still reporting this
           problem, then perhaps the problem is that Emacs is not
           looking in the right place for the executable. The
           list of directories that Emacs looks in are held in
           the Emacs variable `exec-path'. I'll print the current
           value of your `exec-path' in a moment. If it does not
           contain the directory containing your executable, you
           will need to change the value of the `exec-path'
           variable. One way to do that is to run `M-x customize`
           in Emacs, and search for `exec-path'.\n\nHere is the
           current value of your `exec-path'
           variable:\n\n%s" (s-join "\n\n" exec-path)))
        (mac-install-help
         "For MacOS, in order to have support for displaying
          LaTeX preview images on \"Retina\" screens, I recommend
          using the emacs-mac package from Homebrew (aka
          Mitsuharu Emacs, see
          https://github.com/railwaycat/homebrew-emacsmacport/).

          Please note that with a \"Retina\" screen, LaTeX
          preview images will be fuzzy with any of the following:
          - GNU Emacs from https://www.gnu.org/software/emacs/
          - The emacs package on Homebrew
          - The emacs-plus package on Homebrew"))

    (unless (>= emacs-major-version 26)
      (push (xenops-doctor-format
             "⚠️ Xenops requires Emacs version >= 26.

              This is due to the dependency on the emacs-aio
              library, but it's good to have a recent Emacs
              version. Your Emacs version is %s.

              For Linux or Windows, see
              https://www.gnu.org/software/emacs/download.html,
              or use your package manager.

              %s" emacs-version mac-install-help)
            problems))

    (unless (executable-find "latex")
      (push (xenops-doctor-format
             "⚠️ Emacs cannot find your `latex` executable.

              Have you installed
              latex (https://www.latex-project.org/get)? To check
              whether it's installed, run the command `which
              latex` in a terminal: it should print out the
              location of the executable.

              %s" exec-path-help) problems))

    (unless (executable-find "dvisvgm")
      (push (xenops-doctor-format
             "⚠️ Emacs cannot find your `dvisvgm` executable. This
              is required in order to convert latex content into
              images for display in the Xenops Emacs buffer.

              dvisvgm is often installed as part of a latex
              installation (try looking for it in the same
              directory as your latex executable). However, it
              can be installed manually from
              https://dvisvgm.de/Downloads/. To check whether
              it's installed, run the command `which dvisvgm` in
              a terminal: it should print out the location of the
              executable.

              %s" exec-path-help) problems))

    (unless (image-type-available-p 'svg)
      (push (xenops-doctor-format
             "⚠️ Your Emacs does not support SVG images. Xenops
             requires SVG support because it improves the
             appearance of rendered mathematical content.

             You will have this problem if you are using MacOS
             and you are using default homebrew Emacs or \"Emacs
             For Mac OS X\".

             %s" mac-install-help) problems))

    (unless window-system
      (push (xenops-doctor-format
             "⚠️ You are running Emacs as a terminal application.

              To use Xenops, you must run Emacs as a GUI
              application. This is because Xenops displays images
              in the Emacs buffer. A common way to run Emacs as a
              GUI application is to click on the Emacs
              application icon. If you are starting emacs from
              the command line, make sure you are not using the
              `--no-window-system` or `-nw` command line
              arguments.") problems))

    (let ((buf (get-buffer-create "*Xenops-Doctor*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert
         (if problems
             (s-join
              "\n\n--------------------------------------------------------------------------------\n"
              (nreverse problems))
           "✅ Xenops is healthy!"))
        (fill-region (point-min) (point-max)))
      (if (or problems (not quiet)) (display-buffer buf)))))

(defun xenops-doctor-format (s &rest args)
  "Format string S for `xenops-doctor' output using format arguments ARGS."
  (apply #'format (s-replace-regexp "\n  +" "\n" s) args))

(provide 'xenops-doctor)

;;; xenops-doctor.el

(provide 'xenops-doctor)

;;; xenops-doctor.el ends here
