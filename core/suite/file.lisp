(defpackage #:rove/core/suite/file
  (:use #:cl)
  (:export #:resolve-file
           #:system-files
           #:file-package))
(in-package #:rove/core/suite/file)

(defun compile-file-p (pathname)
  (check-type pathname pathname)
  (equal (pathname-type pathname)
         (uiop:compile-file-type)))

(defun resolve-file (pathname)
  (block nil
    (unless pathname
      (return nil))
    (let ((pathname (uiop:ensure-absolute-pathname pathname)))
      (unless (compile-file-p pathname)
        (return pathname))
      (unless asdf:*user-cache*
        (return pathname))
      (if (eql (search (namestring asdf:*user-cache*)
                       (namestring pathname))
               0)
          (make-pathname
           :type "lisp"
           :defaults pathname
           :directory (cons :absolute
                            (nthcdr (length (pathname-directory asdf:*user-cache*))
                                    (pathname-directory pathname))))
          (uiop:lispize-pathname pathname)))))

(defun system-component-p (system-name component-name)
  (and (< (length system-name) (length component-name))
       (string= system-name
                component-name
                :end2 (length system-name))))

(defun component-source-files (component)
  (typecase component
    (asdf:cl-source-file (list (asdf:component-pathname component)))
    ((or asdf:module
         asdf:system)
     (mapcan #'component-source-files
             (copy-seq (asdf:component-children component))))))

(defun package-inferred-system-component-names (system-designator)
  (let ((system (asdf:find-system system-designator)))
    (let ((already-seen (make-hash-table :test 'equal))
          (deps (mapcar #'string-downcase (asdf:component-sideway-dependencies system)))
          (system-name (asdf:component-name system)))
      (let ((system-component-names
              (remove-if-not (lambda (name)
                               (and (not (gethash name already-seen))
                                    (system-component-p system-name name)))
                             deps)))
        (dolist (comp system-component-names)
          (setf (gethash comp already-seen) t))
        (append (mapcan #'package-inferred-system-component-names system-component-names)
                system-component-names)))))

(defun package-inferred-system-files (system)
  (mapcar (lambda (name)
            (let ((system (asdf:find-system name)))
              (asdf:component-pathname
               (first (asdf:component-children system)))))
          (package-inferred-system-component-names system)))

(defun system-files (system)
  (etypecase system
    (asdf:package-inferred-system
     (package-inferred-system-files system))
    (asdf:system (component-source-files system))))

(defvar *file-package*
  (make-hash-table :test 'equal))

(defun file-package (file &optional (warn t))
  (let ((package (gethash file *file-package*)))
    (when (and (null package)
               warn)
      (warn "No package found for file '~A'." file))))

(defun (setf file-package) (package file)
  (setf (gethash file *file-package*) package))

(defun system-packages (system)
  (let ((files (system-files system)))
    (remove-duplicates
     (remove nil
             (mapcar (lambda (file)
                       (gethash file *file-package*))
                     files))
     :from-end t)))
