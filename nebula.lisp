;;;; nebula.lisp

(in-package #:nebula)

;;; "nebula" goes here. Hacks and glory await!

(defvar *nebula-path* "nebula-store")

(defun resolve-target (uuid)
  "Given a UUID, follow its targets all the way to the underlying
blob."
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (let ((target (entry-target entry)))
	(cond
	  ((hash-p target) (read-blob *nebula-path* target))
	  ((uuid-p target) (resolve-target target))
	  (:t nil))))))

(defun upload-blob (data &key (parent nil))
  "Upload a blob of data, possibly under a parent entry."
  (let* ((hash  (write-blob *nebula-path* data))
	 (entry (make-entry hash parent)))
    (unless (null entry)
      (store-entry entry)
      (entry-uuid entry))))

(defun entry-info (uuid)
  "Retrieve metadata about an entry."
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (st-json:write-json-to-string
       (st-json:jso
	"id"      (entry-uuid entry)
	"created" (entry-created entry)
	"parent"  (entry-parent entry))))))

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

(defun entry-history (uuid)
  "Return a list of entry IDs of this entry's lineage, from this entry
to the parent.n"
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (load-history entry))))

(defun remove-entry (uuid)
  (delete-entry *nebula-path* uuid))

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
