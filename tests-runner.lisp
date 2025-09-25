(ql:quickload :cl-coverage-reporter.cli)

(cl-coverage-tools:enable-coverage)

(push *default-pathname-defaults* ql:*local-project-directories*)

(ql:quickload :proper-name.test)

(defun run-tests (coverage)
  (5am:run-all-tests)
  (when coverage
    (cl-coverage-reporter.cli:report (list :proper-name))))

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore c h))
        (uiop:quit -1))
      fiveam:*on-error* nil)

(unless (run-tests t)
  (exit :code 1 :abort t))
