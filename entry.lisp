(in-package #:nebula)

(defclass entry ()
  ((uuid    :col-type string  :initarg :uuid    :accessor entry-uuid)
   (target  :col-type string  :initarg :target  :accessor entry-target)
   (created :col-type integer :initarg :created :accessor entry-created)
   (size    :col-type integer :initarg :size    :accessor entry-size)
   (parent  :col-type string  :initarg :parent  :accessor entry-parent))
  (:metaclass postmodern:dao-class) ; make this a database access object
  (:keys uuid)
  (:doc  "An entry associates some notion of a blob with additional data."))

(defun make-entry (hash parent size)
  (when (and (hash-p hash)
	     (valid-parent-p parent))
    (make-instance 'entry
		   :uuid    (gen-uuid)
		   :target  hash
		   :created (time-now)
		   :size    size
		   :parent  (if (null parent) "" parent))))

(defgeneric proxy-entry (parent size))
(defmethod proxy-entry (parent size)
  (declare (ignore parent size))
  nil)

(defmethod proxy-entry ((parent entry) (size integer))
  (make-instance 'entry
		   :uuid    (gen-uuid)
		   :target  (entry-uuid parent)
		   :created (entry-created parent)
		   :size    (entry-size parent)
		   :parent  ""))

(defun entry-p (e)
  "Is this an entry?"
  (eql (type-of e) 'entry))
