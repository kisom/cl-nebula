(in-package #:nebula)

(defun hash-data (data)
    "Produce the hex-encoded SHA-256 digest of data."
    (let ((data (if (byte-string-p data)
		    data
		    (flexi-streams:string-to-octets data))))
      (ironclad:byte-array-to-hex-string
       (ironclad:digest-sequence :sha256 data))))

(defun hash-path (parent digest)
  "Produce a Nebula CAS path for the digest."
  (pathname
   (concatenate 'string parent (split-path digest))))

(defun split-path (digest)
  (if (string-emptyp  digest) ""
      (concatenate 'string "/"
		   (subseq digest 0 2)
		   (split-path
		    (subseq digest 2)))))

(defun write-blob (parent data)
  (let* ((digest (hash-data data))
	 (path   (hash-path parent digest)))
    (ensure-directories-exist path)
    (with-open-file (s path
		       :direction :output
		       :if-exists nil)
      (with-standard-io-syntax
	(princ data s)))
    digest))

(defmacro define-file-action (name action)
  `(defun ,name (parent digest)
     (let ((path (hash-path parent digest)))
       (when (probe-file path)
	 (funcall ,action path parent)))))

(defun purge-top (path top)
  (unless (equalp path top)
    (and (null (uiop:directory-files path))
	 (uiop:delete-empty-directory path)
	 (purge-top (uiop:pathname-parent-directory-pathname path) top))))

(defun purge-file (path parent)
  (delete-file path)
  (purge-top (uiop:pathname-directory-pathname path)
	     (pathname (concatenate 'string parent "/"))))

(define-file-action read-blob #'read-file)
(define-file-action delete-blob #'purge-file)




