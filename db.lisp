(in-package #:nebula)

(defvar *db-creds-path* (merge-pathnames
			 (user-homedir-pathname)
			 "nebula.lisp"))

(defvar *db-creds*)

(defun load-credentials (&key (path *db-creds-path*))
  (let ((creds (with-open-file (f path)
		 (with-standard-io-syntax
		   (read f)))))
    (unless (null creds)
      (setf *db-creds* creds))))

(defmacro cred-value (k)
  `(value-for *db-creds* ,k))

(defun connect ()
  (when (boundp '*db-creds*)
    (when (boundp 'postmodern:*database*)
      (postmodern:disconnect postmodern:*database*))
    (postmodern:with-connection (list (cred-value :db-name)
				       (cred-value :db-user)
				       (cred-value :db-pass)
				       (cred-value :db-host)
				       :port (cred-value :db-port)
				       :pooled-p t)
      (unless (postmodern:table-exists-p 'entry)
	(postmodern:execute
	 (postmodern:dao-table-definition 'entry))
	t))))

(defmacro defun-with-db (name args &body body)
  `(defun ,name ,args
     (postmodern:with-connection (list (cred-value :db-name)
				       (cred-value :db-user)
				       (cred-value :db-pass)
				       (cred-value :db-host)
				       :port (cred-value :db-port)
				       :pooled-p t)
       ,@body)))

(defun-with-db store-entry (ent)
  (when (entry-p ent)
    (postmodern:insert-dao ent)))

(defun select-by-target (identifier)
  (postmodern:select-dao 'entry (:= 'target identifier)))

(defun garbage-collect-blob (path entry)
  (when (hash-p (entry-target entry))
    (let ((others (select-by-target (entry-target entry))))
      (when (zerop (length others))
	(delete-blob path (entry-target entry))))))

(defun garbage-collect-references (entry)
  (dolist (ent (select-by-target (entry-uuid entry)))
    (garbage-collect-references ent)
    (postmodern:delete-dao ent)))

(defun clear-children (entry)
  (let ((target (entry-uuid entry)))
    (dolist (ent (postmodern:select-dao 'entry (:= 'parent target)))
      (setf (entry-parent ent) "")
      (postmodern:update-dao ent))))

(defun-with-db lookup-entry (uuid)
  (when (uuid-p uuid)
    (postmodern:get-dao 'entry uuid)))

(defun-with-db delete-entry (path uuid)
  (let ((entry (lookup-entry uuid)))
    (unless (null entry)
      (postmodern:delete-dao entry)
      (garbage-collect-blob path entry)
      (garbage-collect-references entry)
      (clear-children entry)
      t)))


