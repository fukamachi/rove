(in-package #:cl-user)
(defpackage #:rove/core/suite
  (:use #:cl
        #:rove/core/assertion
        #:rove/core/test
        #:rove/core/conditions)
  (:export #:run-system-tests))
(in-package #:rove/core/suite)

(defun system-dependencies (system)
  (unless (typep system 'asdf:package-inferred-system)
    (error "~A isn't a package-inferred-system" system))

  (let* ((deps (asdf:component-sideway-dependencies system))
         (deps
           (remove-if-not (lambda (system)
                            (typep system 'asdf:package-inferred-system))
                          (mapcar #'asdf:find-system deps))))
    (remove-duplicates
     (append (mapcan #'system-dependencies deps)
             deps)
     :from-end t)))

(defun system-component-p (system component)
  (let* ((system-name (asdf:component-name system))
         (comp-name (asdf:component-name component)))
    (and (< (length system-name) (length comp-name))
         (string= system-name
                  comp-name
                  :end2 (length system-name)))))

(defun run-system-tests (system-designator)
  (let ((system (asdf:find-system system-designator)))
    (unless (typep system 'asdf:package-inferred-system)
      (error "~A isn't a package-inferred-system" system))

    (let ((passed '())
          (failed '()))
      (handler-bind ((package-tests-passed
                       (lambda (c)
                         (push (package-tests-name c) passed)))
                     (package-tests-failed
                       (lambda (c)
                         (push (package-tests-name c) failed))))
        (let ((deps (remove-if-not (lambda (dep)
                                     (system-component-p system dep))
                                   (system-dependencies system))))
          (dolist (dep deps)
            (let* ((package-name (string-upcase (asdf:component-name dep)))
                   (package (find-package package-name)))
              (when package
                (clear-package-tests package))

              #+quicklisp (ql:quickload (asdf:component-name dep) :silent t)

              (dolist (c (asdf:component-children dep))
                (when (typep c 'asdf:cl-source-file)
                  (load (asdf:component-pathname c) :verbose t)))

              (unless package
                (or (setf package (find-package package-name))
                    (error "Package ~A not found" package-name)))
              (run-package-tests package))))

        (let* ((package-name (string-upcase (asdf:component-name system)))
               (package (find-package  package-name)))
          (when package
            (clear-package-tests package))

          #+quicklisp (ql:quickload package-name :silent t)

          ;; Loading CL-SOURCE-FILE
          (dolist (c (asdf:component-children system))
            (when (typep c 'asdf:cl-source-file)
              (load (asdf:component-pathname c) :verbose t)))

          (unless package
            (or (setf package (find-package package-name))
                (error "Package ~A not found" package-name)))
          (when (package-tests package)
            (run-package-tests package))))

      (values (not failed)
              (nreverse passed)
              (nreverse failed)))))
