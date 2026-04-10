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

(defun multithread-suffix-p (impl)
  (and (stringp impl)
       (<= 2 (length impl))
       (string= impl "-s" :start1 (- (length impl) 2))))

;; Cached result of computing the ASDF output translation root.
;; :not-computed means the value has not been determined yet.
(defvar *effective-fasl-cache-root* :not-computed)

(defun effective-fasl-cache-root ()
  "Return the root directory under which ASDF stores compiled files
according to the current output translation configuration.
When ASDF_OUTPUT_TRANSLATIONS points to a custom cache, this returns that
custom root rather than the default asdf:*user-cache*.
Returns nil if the translation appears to be an identity (no separate cache).

The probe pathname is intentionally one directory component deep so that
butlast peeling exactly (length source-comps) components isolates the prefix.
This assumes a single uniform translation rule applies to all source files."
  (when (eq *effective-fasl-cache-root* :not-computed)
    (setf *effective-fasl-cache-root*
          (handler-case
              (let* ((probe (make-pathname :directory '(:absolute "rove--fasl-probe")
                                           :name "probe" :type "lisp"))
                     (translated (asdf:apply-output-translations probe))
                     (source-comps (cdr (pathname-directory probe)))
                     (trans-comps (cdr (pathname-directory translated)))
                     (cache-comps (butlast trans-comps (length source-comps))))
                (when (and cache-comps (every #'stringp cache-comps))
                  (make-pathname :directory (cons :absolute cache-comps)
                                 :name nil :type nil :version nil
                                 :defaults translated)))
            (error ()
              nil))))
  *effective-fasl-cache-root*)

(defun resolve-file (pathname)
  (block nil
    (unless pathname
      (return nil))
    (let ((pathname (uiop:ensure-absolute-pathname pathname)))
      (unless (compile-file-p pathname)
        (return pathname))
      ;; Determine the actual cache root: prefer the dynamically probed root
      ;; (which honours ASDF_OUTPUT_TRANSLATIONS) over *user-cache*.
      (let* ((cache-root (or (effective-fasl-cache-root)
                             asdf:*user-cache*)))
        (unless cache-root
          (return pathname))
        (let* ((fasl-str (namestring pathname))
               (impl (uiop:implementation-identifier))
               (multithread-root
                 ;; UIOP may add a -s suffix for multithreaded SBCL.  Some FASLs
                 ;; were compiled under the suffix-less variant of the cache dir.
                 ;; Use cache-root (not *user-cache*) as the base so that custom
                 ;; ASDF_OUTPUT_TRANSLATIONS are respected here too.
                 (when (and (uiop:featurep :sb-thread)
                            (multithread-suffix-p impl))
                   (merge-pathnames
                    (subseq impl 0 (- (length impl) 2))
                    (uiop:pathname-parent-directory-pathname cache-root))))
               (match-root
                 (cond
                   ((eql (search (namestring cache-root) fasl-str) 0)
                    cache-root)
                   ((and multithread-root
                         (eql (search (namestring multithread-root) fasl-str) 0))
                    multithread-root))))
          (if match-root
              (let* ((directories (nthcdr (length (pathname-directory match-root))
                                          (pathname-directory pathname)))
                     (device (pathname-device pathname))
                     (device (when (and device (not (eq device :unspecific)))
                               (pop directories))))
                (if (or device directories)
                    (make-pathname
                     :type "lisp"
                     :defaults pathname
                     :device device
                     :directory (cons :absolute directories))
                    (uiop:lispize-pathname pathname)))
              (progn
                (warn "resolve-file: could not map FASL ~A back to its source file. ~
                       ASDF_OUTPUT_TRANSLATIONS may point to a location that rove's ~
                       cache-root probe did not detect.  Run with a uniform single-rule ~
                       output translation or ensure *user-cache* is set."
                      pathname)
                (uiop:lispize-pathname pathname))))))))


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
  (let ((package (gethash (uiop:native-namestring file) *file-package*)))
    (when (and (null package)
               warn)
      (warn "No package found for file '~A'." file))
    package))

(defun (setf file-package) (package file)
  (setf (gethash (uiop:native-namestring file) *file-package*) package))

(defun system-packages (system)
  (let ((files (system-files system)))
    (remove-duplicates
     (remove nil
             (mapcar (lambda (file)
                       (file-package file nil))
                     files))
     :from-end t)))
