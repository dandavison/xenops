;; -*- lexical-binding: t -*-

(defun xenops-aio-subprocess (command &optional output-buffer error-buffer)
  "Start asynchronous subprocess; return a promise.

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
                           (prog1 (list :xenops-error-data
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