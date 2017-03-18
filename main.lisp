(in-package #:cl-user)
(defpackage #:rove/main
  (:nicknames #:rove)
  (:use #:cl)
  (:import-from #:rove/core/assertion
                #:*debug-on-error*
                #:ok
                #:ng
                #:signals
                #:outputs
                #:pass
                #:fail
                #:skip)
  (:import-from #:rove/core/test
                #:deftest
                #:testing)
  (:import-from #:rove/core/suite
                #:run-system-tests)
  (:import-from #:rove/core/stats
                #:*stats*)
  (:import-from #:rove/core/result
                #:form-description)
  (:import-from #:rove/reporter
                #:with-reporter
                #:diag
                #:*report-stream*)
  (:import-from #:rove/misc/color
                #:*enable-colors*)
  (:import-from #:rove/reporter/spec
                #:spec-reporter)
  (:export #:*debug-on-error*
           #:ok
           #:ng
           #:signals
           #:outputs
           #:pass
           #:fail
           #:skip
           #:deftest
           #:testing
           #:run

           #:form-description

           #:*stats*

           #:diag
           #:with-reporter
           #:*report-stream*
           #:*enable-colors*))
(in-package #:rove/main)

(defun run (target &key (style :spec))
  (with-reporter style
    (run-system-tests target)))
