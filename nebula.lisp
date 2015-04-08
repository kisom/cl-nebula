;;;; nebula.lisp

(in-package #:nebula)

;;; "nebula" goes here. Hacks and glory await!

(defvar *nebula-path* "nebula-store")

(defun set-store-path (path)
  "Provide an alternative path for the blob store."
  (setq *nebula-path* path))

(defun retrieve (uuid)
  "Given a UUID, follow its targets all the way to the underlying
blob."
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (let ((target (entry-target entry)))
	(cond
	  ((hash-p target) (read-blob *nebula-path* target))
	  ((uuid-p target) (resolve-target target))
	  (:t nil))))))

(defun store (data &key parent)
  "Store some data, possibly under a parent entry."
  (when (valid-parent-p parent)
    (let* ((hash  (write-blob *nebula-path* data))
	   (entry (make-entry hash parent)))
      (unless (null entry)
	(store-entry entry)
	(entry-uuid entry)))))

(defun info (uuid)
  "Retrieve metadata about an entry."
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (pairlis '(:id :created :parent)
	       (list (entry-uuid entry)
		     (entry-created entry)
		     (entry-parent entry))))))

(defun proxy (uuid)
  "Proxy a single entry."
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (let ((proxied (proxy-entry entry)))
	(store-entry proxied)
	(entry-uuid proxied)))))

(defun load-history (entry)
  (if (string-emptyp (entry-parent entry))
      (list (entry-uuid entry))
      (let ((parent (lookup-entry (entry-parent entry))))
	(when (entry-p parent)
	  (cons (entry-uuid entry) (load-history parent))))))

(defun lineage (uuid)
  "Return a list of entry IDs of this entry's lineage, from this entry
to the parent."
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (load-history entry))))

(defun expunge (uuid)
  "Expunge the entry named by UUID from the system, garbage collecting
as required."
  (when (uuid-p uuid)
    (delete-entry *nebula-path* uuid)))

(defun build-proxied (remaining proxied)
  (if (null remaining)
      proxied
      (let* ((entry (lookup-entry (first remaining)))
	     (proxy (proxy-entry entry)))
	(unless (null proxied)
	  (setf (entry-parent proxy) (first proxied))
	  (store-entry proxy))
	(build-proxied (rest remaining)
		       (cons (entry-uuid proxy) proxied)))))

(defun proxy-all (uuid)
  "Proxy the entire lineage for an entry."
  (let ((lineage (reverse (entry-history uuid))))
    (unless (null lineage)
      (build-proxied lineage nil))))

(defun initialize (&key cred-path store-path)
  "Conducts the necessary setup to begin using nebula."
  (when cred-path
    (setq *db-creds-path* cred-path))
  (when store-path
    (set-store-path store-path))
  (load-credentials)
  (connect))
