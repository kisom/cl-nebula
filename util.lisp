(in-package #:nebula)

(defun byte-string-p (str)
  "Predicate returning true if its argument is a byte string."
  (and (arrayp str)
       (every #'integerp str)))

(defun string-emptyp (s)
    (equal 0 (length s)))

(defun read-file (path &rest args)
  "Read a file as unsigned bytes."
  (declare (ignore args))
  (with-open-file (stream path
			  :element-type '(unsigned-byte 8))
    (let ((data (make-sequence '(vector (unsigned-byte 8)) (file-length stream))))
      (read-sequence data stream)
      data)))

(defun time-now ()
  (local-time:timestamp-to-unix (local-time:now)))
      
(defun gen-uuid ()
  "Generate a random V4 UUID."
  (let ((uuid (ironclad:byte-array-to-hex-string
	       (uuid:uuid-to-byte-array
		(uuid:make-v4-uuid)))))
    (concatenate 'string
		 (subseq uuid 0 8)   "-"
		 (subseq uuid 8 12)  "-"
		 (subseq uuid 12 16) "-"
		 (subseq uuid 16 20) "-"
		 (subseq uuid 20))))

(defun hash-p (possible-match) (ppcre:scan "^[a-f0-9]{64}" possible-match))
(defun uuid-p (possible-match)
  (ppcre:scan "^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$"
	     possible-match))

(defun valid-parent-p (possible-parent)
  (or (null   possible-parent)
      (uuid-p possible-parent)))

(defun value-for (alist key)
  (let ((kv (assoc key alist)))
    (unless (null kv)
      (second kv))))

(defun file-size (path)
  (if (probe-file path)
      (with-open-file (file path)
	(file-length file))
      0))

