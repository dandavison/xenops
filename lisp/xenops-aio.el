;;; xenops-aio.el --- Xenops-aio integration -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Helper functions for working with aio.

;;; Code:
(require 'aio)
(require 's)

(defmacro xenops-aio-with-async-with-buffer (buffer &rest body)
  "Evaluate BODY under `aio-with-async' with current buffer set to BUFFER."
  `(aio-with-async
     (with-current-buffer ,buffer
       ,@body)))

(defun xenops-aio-sem-cancel-waiting-tasks (sem init)
  "Cancel all tasks waiting in the queue and reinitialize semaphore SEM to INIT."
  (setf (aref sem 2) '(nil . nil)
        (aref sem 1) init))

(defun xenops-aio-subprocess (command &optional _ __)
  "Start asynchronous subprocess; return a promise.

COMMAND is the command to run as an asynchronous subprocess.

Resolve the promise when the process exits. The value function
does nothing if the exit is successful, but if the process exits
with an error status, then the value function signals the error."
  (let* ((promise (aio-promise))
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

(provide 'xenops-aio)

;;; xenops-aio.el ends here
