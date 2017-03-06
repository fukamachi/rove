(in-package #:cl-user)
(defpackage #:rove/main
  (:nicknames #:rove)
  (:use #:cl)
  (:import-from #:rove/core/assertion
                #:ok
                #:ng
                #:signals
                #:outputs
                #:pass
                #:fail)
  (:import-from #:rove/core/test
                #:deftest)
  (:import-from #:rove/core/suite
                #:run-system-tests)
  (:import-from #:rove/core/stats
                #:*stats*)
  (:import-from #:rove/reporter
                #:with-reporter
                #:*report-stream*)
  (:import-from #:rove/misc/color
                #:*enable-colors*)
  (:import-from #:rove/reporter/spec
                #:spec-reporter)
  (:export #:ok
           #:ng
           #:signals
           #:outputs
           #:pass
           #:fail
           #:deftest
           #:run

           #:*stats*

           #:with-reporter
           #:*report-stream*
           #:*enable-colors*))
(in-package #:rove/main)

(defun run (target &key (style :spec))
  (with-reporter style
    (run-system-tests target)))
