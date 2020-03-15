;;; xenops-aio.el --- Xenops-aio integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defmacro xenops-aio-with-async-with-buffer (buffer &rest body)
  "Evaluate BODY under `aio-with-async' with current buffer set to BUFFER."
  `(aio-with-async
     (with-current-buffer ,buffer
       ,@body)))

(defun xenops-aio-sem-cancel-waiting-tasks (sem init)
  "Cancel all tasks waiting in the queue and reinitialize the semaphore."
  (setf (aref sem 2) '(nil . nil)
        (aref sem 1) init))

(defun xenops-aio-subprocess (command &optional output-buffer error-buffer)
  "Start asynchronous subprocess; return a promise.

Resolve the promise when the process exits. The value function
does nothing if the exit is successful, but if the process exits
with an error status, then the value function signals the error."
  (let* ((promise (xenops-aio-promise))
         (name (format "xenops-aio-subprocess-%s"
                       (sha1 (prin1-to-string command))))
         (output-buffer (generate-new-buffer name))
         (sentinel
          (lambda (process event)
            (unless (process-live-p process)
              (aio-resolve
               promise
               (lambda ()
                 (if (eq 0 (process-exit-status process))
                     (kill-buffer output-buffer)
                   (signal 'error
                           (prog1 (list :xenops-aio-subprocess-error-data
                                    (list (s-join " " command)
                                          event
                                          (with-current-buffer output-buffer
                                            (buffer-string))))
                             (kill-buffer output-buffer))))))))))
    (prog1 promise
      (make-process
       :name name
       :buffer output-buffer
       :command command
       :sentinel sentinel))))

(if (fboundp 'record)
    (progn
      (defalias 'xenops-aio-promise #'aio-promise)
      (defalias 'xenops-aio-sem #'aio-sem))

  ;; Shims for Emacs <26
  (cl-defstruct aio-promise result callbacks)
  (cl-defstruct aio-sem value waiting-functions)

  (defun xenops-aio-promise ()
    (make-aio-promise :result nil :callbacks ()))

  (defun xenops-aio-sem (init)
    (make-aio-sem :value init :waiting-functions (cons nil nil))))

(provide 'xenops-aio)

;;; xenops-aio.el ends here
