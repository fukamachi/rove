(defpackage #:rove/utils/reporter
  (:use #:cl
        #:rove/core/stats
        #:rove/core/assertion
        #:rove/core/result
        #:rove/misc/stream
        #:rove/misc/color)
  (:import-from #:rove/core/source-location
                #:source-location-file-position)
  (:export #:format-failure-tests))
(in-package #:rove/utils/reporter)

(defun print-source-location-as-file-path (stream file line column)
  (format stream "~&at ~A~@[:~A~]~@[:~A~]~%"
          (enough-namestring file)
          line column))

(defun print-source-location-as-github-url (stream file line)
  (when (uiop:getenv "GITHUB_ACTIONS")
    (format stream "~&at https://github.com/~A/blob/~A/~A#L~A~%"
            (uiop:getenv "GITHUB_ACTION_REPOSITORY")
            (uiop:getenv "GITHUB_REF_NAME")
            (enough-namestring file
                               (uiop:ensure-directory-pathname (uiop:getenv "GITHUB_WORKSPACE")))
            line)))

(defun print-source-location (stream assertion &key (type :file))
  (let ((source-location (assertion-source-location assertion)))
    (when source-location
      (destructuring-bind (file line column)
          (source-location-file-position source-location)
        (ecase type
          (:file (print-source-location-as-file-path stream file line column))
          (:github (print-source-location-as-github-url stream file line)))))))

(defun format-failure-tests (stream passed-tests failed-tests pending-tests)
  (fresh-line stream)
  (write-char #\Newline stream)
  (let ((stream (make-indent-stream stream)))
    (let ((test-count (+ (length passed-tests)
                         (length failed-tests)
                         (length pending-tests))))
      (if (= 0 (length failed-tests))
          (princ
           (color-text :green
                       (format nil "✓ ~D test~:*~P completed"
                               (length passed-tests)))
           stream)
          (progn
            (princ
             (color-text :red
                         (format nil "× ~D of ~D test~:*~P failed"
                                 (length failed-tests)
                                 test-count))
             stream)
            (let ((failed-tests
                    (labels ((assertions (object)
                               (typecase object
                                 (failed-assertion (list object))
                                 (failed-test
                                  (apply #'append
                                         (mapcar #'assertions
                                                 (failed-tests object)))))))
                      (loop for test in failed-tests
                            append (assertions test)))))
              (let ((*print-circle* t)
                    (*print-assertion* t))
                (loop for i from 0
                      for f in failed-tests
                      do (fresh-line stream)
                         (write-char #\Newline stream)
                         (princ
                          (color-text :white
                                      (format nil "~A) ~A"
                                              i
                                              (if (assertion-labels f)
                                                  (with-output-to-string (s)
                                                    (loop for i from 0
                                                          for (label . rest) on (assertion-labels f)
                                                          do (princ (make-string (* i 2) :initial-element #\Space) s)
                                                             (when (< 0 i)
                                                               (princ "   › " s))
                                                             (princ label s)
                                                             (fresh-line s)))
                                                  (assertion-description f))))
                          stream)
                         (when (assertion-labels f)
                           (with-indent (stream (+ (length (write-to-string i)) 2))
                             (fresh-line stream)
                             (princ
                              (color-text :white
                                          (assertion-description f))
                              stream)
                             (with-indent (stream +4)
                               (print-source-location stream f
                                                      :type
                                                      (if (uiop:getenv "GITHUB_ACTIONS")
                                                          :github
                                                          :file)))))
                         (fresh-line stream)
                         (with-indent (stream (+ (length (write-to-string i)) 2))
                           (when (assertion-reason f)
                             (princ
                              (color-text :red
                                          (format nil "~A: ~A"
                                                  (type-of (assertion-reason f))
                                                  (assertion-reason f)))
                              stream)
                             (fresh-line stream))
                           (with-indent (stream +2)
                             (princ
                              (color-text :gray (prin1-to-string f))
                              stream)
                             (fresh-line stream)
                             (when (assertion-stacks f)
                               (write-char #\Newline stream)
                               (let ((*print-circle* nil))
                                 (ignore-errors
                                  (loop repeat 15
                                        for stack in (assertion-stacks f)
                                        do (princ (color-text :gray (dissect:present-object stack nil)) stream)
                                           (fresh-line stream))))))))))))))
  (fresh-line stream)
  (unless (= 0 (length pending-tests))
    (princ
     (color-text :aqua
                 (format nil "● ~D test~:*~P skipped"
                         (length pending-tests)))
     stream)
    (fresh-line stream)))
