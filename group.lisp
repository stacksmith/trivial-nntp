;; Interacting with groups
 #|



|#

(in-package :trivial-nntp)

;;;
;;; A group structure, to keep track of a newsgroup on a server...
;;;
(defstruct group name high low )

(defun group-count (group)
  "return count of messages in group"
  (- (group-high group) (group-low group) ))

;; In response to "LIST" command, the server sends
;; groupname HIGH LOW y/n.  Minimum
(defun string-to-group (string &key (minimum 10))
  "parse a single LIST response string into a group structure; specify minimum number of messages"
  (multiple-value-bind (a b start end)
      (cl-ppcre:scan "([^\\s]+)\\s+(\\d+)\\s+(\\d+)" string)
    (declare (ignore a b)) ;; start and end of scan are unused
    (let ((high (parse-integer string :start (elt start 1) :end (elt end 1)))
	  (low  (parse-integer string :start (elt start 2) :end (elt end 2))))
      (if (> high (+ minimum low)) ;; only work with groups that have posts
	  (make-group :name (subseq string (elt start 0) (elt end 0))
		:high high :low low)
	  nil))))

;;; load groups into a list
(defparameter *groups* nil;;(make-hash-table :test 'equal)
)

(defun fetch-groups (connection)
  "fetch all  groups from the server"
  (send-command connection "LIST ACTIVE" :expecting 2)
  (setf *groups* (read-list connection :proc #'string-to-group))
)

(defun find-group (name)
  "find a group struct by name"
  (find name *groups* :test #'(lambda (a b) (equal a (group-name b)))))

(defun search-groups (name)
  "search grouplist for a regex and return list"
  (loop for group in *groups*
     when (cl-ppcre:scan name (group-name group))
     collect group))

(defun sort-groups-by-count ()
  (sort *groups* (lambda (a b) (> (group-count a) (group-count b)) ))
  )


(defun save-form (filename)
  (with-open-file
      (out (merge-pathnames filename (asdf:system-source-directory 'trivial-nntp)))
    :direction :output
    :if-exists :supersede
    :if-does-not-exist :create
    (write *groups* :stream out)))

(defun load-form (filename)
  (with-open-file
      (in (merge-pathnames filename (asdf:system-source-directory 'trivial-nntp)))
    :direction :input
    (setf *groups* (read in))))

(defun enter-group (connection groupname)
  "enter a group and retreive a list of article ids"
  (send-command connection "LISTGROUP" :also groupname :expecting 2)
  )

(defun cmd-xover (connection grp)
  (send-command connection
   (with-output-to-string (out) (format out "XOVER ~d-~d" (group-low grp) (group-high grp))))
  )

(defstruct xoverline num subject author date message-id references byte-count line-count )
;; When parsing xoverlines, note that references may not be there (see the * instead of the +)!
(defun string-to-xoverline (string)
  (multiple-value-bind (a b start end) 
      (cl-ppcre:scan "([\\d]+)\\t([^\\t]+)\\t([^\\t]+)\\t([^\\t]+)\\t([^\\t]+)\\t([^\\t]*)\\t([^\\t]+)\\t([^\\t])" string)
   ; (format t "~a ~a" start end)
    (declare (ignore a b)) ;; start and end of scan are unused
    (let ((num (parse-integer string :start (elt start 0) :end (elt end 0)))
	  (subject    (subseq string (elt start 1)  (elt end 1)))
	  (author     (subseq string (elt start 2)  (elt end 2)))
	  (date       (subseq string (elt start 3)  (elt end 3)))
	  (message-id (subseq string (elt start 4)  (elt end 4)))
	  (references (subseq string (elt start 5)  (elt end 5)))
	  (byte-count (parse-integer string :start (elt start 6) :end (elt end 6)))
	  (line-count (parse-integer string :start (elt start 7) :end (elt end 7)))
	  
	  )
      (make-xoverline :num num :subject subject :author author :date date
		      :message-id message-id :references references
		      :byte-count byte-count :line-count line-count)
)
    
))


(defun read-xoverlines (connection)
  (read-list connection :proc #'string-to-xoverline))



(defun helper (connection groupname)
  ;; load groups if needed
  (unless *groups*
    (setf *groups* (load-groups connection)))
  ;; need extra information in group structure (min,max)
  (let ((grp (find-group groupname)))
    (when grp
      (send-command connection "GROUP" :also groupname)
      (cmd-xover connection grp)
 )
)
  )


