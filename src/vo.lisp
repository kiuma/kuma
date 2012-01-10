;;; -*- lisp -*-
;;; $Header: vo.lisp $

;;; Copyright (c) 2011, Andrea Chiumenti.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



(in-package :kuma)

(defvar *out* "")
(defvar *out2* "")

(defgeneric closable-close (closable))
(defclass closable () ())
(defmethod closable-close ((closable closable))
  nil)


(defgeneric header-value (header key))

(defgeneric (setf header-value) (value header key))

(defgeneric header-slot-value (header slot))

(defgeneric header-slot-value-as-time (header slot))

(defgeneric header-slot-value-as-number (header slot))

(defgeneric header-slot-multi-value (header slot))

(defun append-to-array-and-shift (ar item)
  (let ((ar-length (length ar)))
    (adjust-array ar (+ 1 ar-length) :initial-element item)
    (make-array ar-length :element-type (array-element-type ar)
		:displaced-to ar
		:displaced-index-offset 1)))

(defclass header ()
  ((headers :accessor header-headers :initarg :headers)
   (headers-buffer :accessor headers-buffer :initform nil))
  (:default-initargs :headers (make-hash-table :test #'equalp)))

(defmethod header-value ((header header) key)
  (gethash (or (and (symbolp key) (symbol-name key)) key)
           (header-headers header)))

(defmethod (setf header-value) (value (header header) key)
  (if value
      (setf (gethash (or (and (symbolp key) (symbol-name key)) key)
                     (header-headers header))
            value)
      (remhash key (header-headers header))))

(defun request-param (key &optional (request *kuma-request*))
  (header-value (http-request-header request) key))

(defmethod header-multi-value ((header header) key)
  (%parse-multi-value (getf (header-headers header) key)))

(defmethod header-slot-value ((header header) slot)
  (if (slot-boundp header slot)
      (slot-value header slot)
      (setf (slot-value header slot)
            (header-value header (intern (symbol-name slot) :keyword)))))

 (defmethod header-slot-value-as-time ((header header) slot)
  (if (slot-boundp header slot)
      (slot-value header slot)      
      (setf (slot-value header slot)
            (let ((time-string (header-value header (intern (symbol-name slot) :keyword))))
              (and time-string (date:parse-time time-string)))))) 

(defmethod header-slot-value-as-number ((header header) slot)
  (if (slot-boundp header slot)
      (slot-value header slot)      
      (setf (slot-value header slot)
            (let ((number-string (header-value header (intern (symbol-name slot) :keyword))))
              (if number-string 
                  (safe-read-from-string number-string)
                  0)))))

(defclass general-header (header)
  ((cache-control :accessor header-cache-control)
   (connection :accessor header-connection)
   (date :accessor header-date)
   (pragma :accessor header-pragma)
   (trailer :accessor header-trailer)
   (transfer-encoding :accessor header-transfer-encoding)
   (upgrade :accessor header-upgrade)
   (via :accessor header-via)
   (warning :accessor header-warning)))

(defmethod header-cache-control ((header general-header))
  (header-slot-value header 'cache-control))

(defmethod header-connection ((header general-header))
  (header-slot-value header 'connection))

(defmethod header-date ((header general-header))
  (header-slot-value-as-time header 'date))

(defmethod header-pragma ((header general-header))
  (header-slot-value header 'pragma))

(defmethod header-trailer ((header general-header))
  (header-slot-value header 'trailer))

(defmethod header-transfer-encoding ((header general-header))
  (header-slot-value header 'transfer-encoding))

(defmethod header-upgrade ((header general-header))
  (header-slot-value header 'upgrade))

(defmethod header-via ((header general-header))
  (header-slot-value header 'via))

(defmethod header-warning ((header general-header))
  (header-slot-value header 'warinig))

(defclass entity-header (header)
  ((allow :accessor header-allow)
   (content-encoding :accessor header-content-encoding)
   (content-language :accessor header-content-language)
   (content-length :accessor header-content-length)
   (content-location :accessor header-content-location)
   (content-md5 :accessor header-content-md5)
   (content-range :accessor header-content-range)
   (content-type :accessor header-content-type)
   (expires :accessor header-expires)
   (last-modified :accessor header-last-modified)))

(defmethod header-allow ((header entity-header))
  (header-slot-multi-value header 'allow))

(defmethod header-content-encoding ((header entity-header))
  (header-slot-value header 'content-encoding))

(defmethod header-content-language ((header entity-header))
  (header-slot-multi-value header 'content-language))

(defmethod header-content-length ((header entity-header))
  (header-slot-value-as-number header 'content-length))

(defmethod header-content-location ((header entity-header))
  (header-slot-value header 'content-location))

(defmethod header-content-md5 ((header entity-header))
  (header-slot-value header 'content-md5))

(defmethod header-content-range ((header entity-header))
  (header-slot-value header 'content-range))

(defmethod header-content-type ((header entity-header))
  (header-slot-value header 'content-type))

(defmethod header-expires ((header entity-header))
  (header-slot-value-as-time header 'expires))

(defmethod header-last-modified ((header entity-header))
  (header-slot-value-as-time header 'last-modified))

(defgeneric header-accept-encoding-p (header encoding))

(defclass request-header (general-header entity-header)
  ((accept :accessor header-accept)
   (accept-charset :accessor header-accept-charset)
   (accept-encoding :accessor header-accept-encoding)
   (accept-language :accessor header-accept-language)
   (authorization :accessor header-authorization)
   (expect :accessor header-expect)
   (from :accessor header-from)
   (host :accessor header-host)
   (if-match :accessor header-if-match)
   (if-modified-since :accessor header-if-modified-since)
   (if-none-match :accessor header-if-none-match)
   (if-range :accessor header-if-range)
   (if-unmodified-since :accessor header-if-unmodified-since)
   (max-forwards :accessor header-max-forwards)
   (proxy-authorization :accessor header-proxy-authorization)
   (range :accessor header-range)
   (referer :accessor header-referer)
   (te :accessor header-te)
   (user-agent :accessor header-user-agent)
   ;(method :accessor header-method)
   ;(request-uri :accessor header-request-uri)
   ;(http-version :accessor header-http-version)
   )
  (:default-initargs :headers nil))

(defmethod header-accept ((header request-header))
  (header-slot-multi-value header 'accept))

(defmethod header-accept-encoding-p ((header request-header) encoding)
  (let ((encodings (header-accept-encoding header)))
    (or (string-equal encoding "identity")
	(member "*" encodings :test #'equal)
	(member-if (lambda (item) (string-equal encoding item))
		   encodings))))

(defmethod haeder-accept-encoding ((header request-header))
  (header-slot-multi-value header 'accept-encoding))

(defmethod header-accept-language ((header request-header))
  (header-slot-multi-value header 'accept-language))

(defmethod header-authorization ((header request-header))
  (header-slot-value header 'authorization))

(defmethod header-expect ((header request-header))
  (header-slot-value header 'expect))

(defmethod header-from ((header request-header))
  (header-slot-value header 'from))

(defmethod header-host ((header request-header))
  (header-slot-value header 'host))

(defmethod header-if-match ((header request-header))
  (header-slot-multi-value header 'if-match))

(defmethod header-if-modified-since ((header request-header))
  (header-slot-value-as-time header 'if-modified-since))

(defmethod header-if-none-match ((header request-header))
  (header-slot-multi-value header 'if-none-match))

(defmethod header-if-range ((header request-header))
  (header-slot-value header 'if-range))

(defmethod header-if-unmodified-since ((header request-header))
  (header-slot-value-as-time header 'if-unmodified-since))

(defmethod header-max-forwards ((header request-header))
  (header-slot-value-as-number header 'max-forwards))

(defmethod header-proxy-authorization ((header request-header))
  (header-slot-value header 'proxy-authorization))

(defmethod header-range ((header request-header))
  (header-slot-value header 'range))

(defmethod header-referer ((header request-header))
  (header-slot-value header 'referer))

(defmethod header-te ((header request-header))
  (header-slot-multi-value header 'te))

(defmethod header-user-agent ((header request-header))
  (header-slot-value header 'user-agent))

#|
(defmethod header-method ((header request-header))
  (header-slot-value header 'method))

(defmethod header-request-uri ((header request-header))
  (header-slot-value header 'request-uri))

(defmethod header-http-version ((header request-header))
  (header-slot-value header 'http-version))
|#
(defclass response-header (general-header entity-header)
  ((accept-ranges :accessor header-accept-ranges :initform nil)
   (age :accessor header-age :initform nil)
   (etag :accessor header-etag :initform nil)
   (location :accessor header-location :initform nil)
   (proxy-authenticate :accessor header-proxy-authenticate :initform nil)
   (retry-after :accessor header-retry-after :initform nil)
   (server :accessor header-server :initform nil)
   (vary :accessor header-vary :initform nil)
   (www-authenticate :accessor header-www-authenticate :initform nil)))

(defgeneric http-request-method (request))
(defgeneric http-request-uri (request))
(defgeneric http-request-http-version (request))
(defgeneric http-request-location (request))
(defgeneric http-request-query-string (request))
(defgeneric add-post-parameter (request param value))
(defgeneric add-get-parameter (request param value))

(defclass http-request (closable)
  ((requset-line :accessor http-request-request-line :initform nil)
   (header :reader http-request-header :initform (make-instance 'request-header))
   (body-entity-read-p :reader http-request-body-entity-read-p :initform nil)
   (get-parameters :reader http-request-get-parameters :initform nil)
   (post-parameters :reader http-request-post-parameters :initform nil)
   (body-boundary :reader http-request-body-boundary :initform nil)
   (done-p :accessor http-request-done-p :initform nil)
   (worker :accessor http-request-worker :initform nil)))

(defmethod add-post-parameter ((request http-request) param value)
  (with-slots ((params post-parameters))
      request
    (setf params (append params (list param value)))))

(defmethod add-get-parameter ((request http-request) param value)
  (with-slots ((params get-parameters))
      request
    (setf params (append params (list param value)))))

(defmethod http-request-method ((request http-request))
  (getf (http-request-request-line request) :method))

(defmethod http-request-uri ((request http-request))
  (getf (http-request-request-line request) :request-uri))

(defmethod http-request-http-version ((request http-request))
  (getf (http-request-request-line request) :http-version))

(defmethod  http-request-location ((request http-request))
  (first (cl-ppcre:split "\\?" (http-request-uri request))))

(defmethod  http-request-query-string ((request http-request))
  (second (cl-ppcre:split "\\?" (http-request-uri request))))

(defclass http-response (closable)
  ((status-line :accessor http-response-status-line :initarg :status-line)
   (response-header :reader http-response-header :initform (make-instance 'response-header))
   (body-content :accessor http-response-body-content :initarg :body-content))
  (:default-initargs :body-content nil :status-line (cons "HTTP/1.1" +http-ok+)))


(defun clear-response-params (&optional (response *kuma-response*))
  (clrhash (header-headers (http-response-header response))))

(defun (setf response-param) (value key &optional (response *kuma-response*))
  (setf (header-value (http-response-header response) key) value))

(defun response-param (key &optional (response *kuma-response*))
  (header-value (http-response-header response) key))

(defun (setf status-line) (code)
  (setf (http-response-status-line *kuma-response*) 
        (cons "HTTP/1.1" (or (and (listp code) code)
                             (list code (getf +http-http-results+ code ""))))))

(defun status-line (&optional (response *kuma-response*))
  (format nil "~{~a~^ ~}" (http-response-status-line response)))

(defun status-line-code (&optional (response *kuma-response*))
  (second (http-response-status-line response)))

(defclass connection ()
  ((client :reader connection-client :initarg :client)
   (who :reader connection-who :initarg :who)
   (port :reader connection-port :initarg :port)
   (io-handler-writer-p :accessor io-handler-writer-p :initform nil)
   (io-handler-reader-p :accessor io-handler-reader-p :initform t)
   (read-buffer :accessor connection-read-buffer :initform (make-array 65536 
								       :element-type '(unsigned-byte 8)
								       :adjustable t))
   (read-buffer-pointer :accessor connection-read-buffer-pointer :initform 0)
   (write-buffer :accessor connection-write-buffer :initform nil)
   (last-access :accessor connection-last-access :initform (get-universal-time))
   (request :accessor connection-request :initform (make-instance 'http-request))
   (request-pipeline :accessor connection-request-pipeline :initform (make-instance 'arnesi:queue))
   (response-reader :accessor connection-response-reader :initform nil)
   (worker-pipeline :accessor connection-worker-pipeline :initform (make-instance 'arnesi:queue))   
   (worker-lock :accessor connection-worker-lock :initform (bt:make-lock))
   (callable :accessor connection-callable :initform nil)
   (callable-lock :accessor connection-callable-lock :initform (bt:make-lock))))


#|
(defgeneric body-stream-handle-buffer (body-stream buffer))

(defclass body-stream (trivial-gray-streams:trivial-gray-stream-mixin)
  ((stream :reader body-stream-stream :initarg :stream)
   (chunkingp :reader body-stream-chunking-p :initarg :chunkingp)
   (compression-method :reader body-stream-compression-method :initarg :compression-method)
   (buffer-length :reader body-stream-buffer-length :initarg :buffer-length)
   (buffer-vector :accessor body-stream-buffer-vector :initform nil))
  (:default-initargs :compression-method nil :buffer-length *default-buffer-size*))

(defmethod initialize-instance :after ((body body-stream) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((stream body-stream-stream)
                   (buffer body-stream-buffer-vector)
                   (buffer-length body-stream-buffer-length)
                   (compression-method body-stream-compression-method))
      body
    (let ((curr-buffer (make-array buffer-length :element-type '(unsigned-byte 8)))
          (bytes-read 0))
      (setf bytes-read (read-sequence curr-buffer stream)
            buffer (body-stream-handle-buffer body (subseq curr-buffer 0 bytes-read))))))

(defmethod body-stream-handle-buffer ((body body-stream) buffer)
  (let* ((method (body-stream-compression-method body))
         (curr-buffer
          (cond
            ((string-equal method "gzip") (salza2:compress-data buffer 'salza2:gzip-compressor))
            ((string-equal method "deflate") (salza2:compress-data buffer 'salza2:deflate-compressor))
            (t buffer))))
    (flexi-streams:make-in-memory-input-stream (concatenate 'vector 
                                                            (babel:string-to-octets (make-http-line (format nil "~x" (length curr-buffer))) 
                                                                                    :encoding :ascii)
                                                            curr-buffer))))
|#

#|
(defmethod stream-read-sequence ((body body-stream) sequence start end &key &allow-other-keys)
  (declare (ignore start end))
  
    (with-accessors ((stream body-stream-stream)
                     (buffer body-stream-buffer-vector)
                     (buffer-length body-stream-buffer-length)
                     (compression-method body-stream-compression-method))
        body
      (let ((bytes-read 0))
        (when buffer
          (progn (setf bytes-read (read-sequence sequence buffer))
                 (when (= bytes-read 0)
                   (let ((curr-buffer (make-array buffer-length :element-type '(unsigned-byte 8)))
                         (buff-bytes-read 0))
                     (setf buff-bytes-read (read-sequence curr-buffer stream)
                           buffer (when (> buff-bytes-read 0) 
                                    (body-stream-handle-buffer body (subseq curr-buffer 0 buff-bytes-read)))))
                   (when buffer (setf bytes-read (read-sequence sequence buffer))))))
        bytes-read)))
|#

(defgeneric complete-response-header (response request))

(defgeneric validate-header-fields (request response))

(defgeneric create-response-header-stream (reader))
(defgeneric create-response-body-reader (reader))
(defgeneric create-response-stream (reader))


(defclass http-response-reader (closable)
  ((request :accessor http-response-reader-request :initarg :request)
   (response :accessor http-response-reader-response :initarg :response)
   (status-line-read-p :accessor status-line-read-p :initform nil)
   (header-read-p :accessor header-read-p :initform nil)
   (body-read-p :accessor body-read-p :initform nil)
   (status-line-stream :accessor status-line-stream :initform nil)
   (header-stream :accessor header-stream :initform nil)
   (body-stream :accessor body-stream :initform nil))
  (:default-initargs :request *kuma-request* :response *kuma-response*))

(defmethod create-response-header-stream ((reader http-response-reader))
  (with-accessors ((response http-response-reader-response) 
                   (request http-response-reader-request))
      reader
    (progn (complete-response-header response request)
           (let ((header-strings (loop for k being the hash-keys in (header-headers (http-response-header response)) using (hash-value v)
                                   collect (make-http-line (format nil "~a: ~a" k v)))))
             (when header-strings
	       (setf *out* (format nil "~%~a~%--------------~%~{~a~}~c~c"
				   (status-line response)
				   header-strings #\Return #\Linefeed))

	       (setf *out2* (format nil "~{~a~}~c~c"				    
				   (loop for k being the hash-keys in (header-headers (http-request-header request)) using (hash-value v)
				      collect (make-http-line (format nil "~a: ~a" k v))) #\Return #\Linefeed))
	       ;;(break "debug")
               (flexi-streams:make-in-memory-input-stream
                (babel:string-to-octets (format nil "~{~a~}~c~c"
						header-strings #\Return #\Linefeed))))))))

(defmethod complete-response-header ((response http-response) (request http-request))
  (let ((*kuma-request* request)
        (*kuma-response* response)
        (body (http-response-body-content response))
        (client-http11-p (string-equal "HTTP/1.1" (http-request-http-version request))))
    (when body
      (when (and (pathnamep body) (or (not (fad:file-exists-p body))
				      (fad:directory-pathname-p body)))
	(signal-http-error +http-not-found+))
      (unless (response-param "Date")
	(setf (response-param "Date")
	      (date:universal-time-to-http-date (get-universal-time))))
      (unless (response-param "Content-Type")
        (cond 
	  ((and (pathnamep body) (fad:file-exists-p body) (not (fad:directory-pathname-p body)))
	   (let ((content-type (or (get-mime body) "application/octet-stream")))
	     (setf (response-param "Content-Type") content-type)))
	  (t (setf (response-param "Content-Type") "text/html"))))
      (unless (response-param "ETag")
        (when (and (pathnamep body) (fad:file-exists-p body) 
                   (not (fad:directory-pathname-p body)) client-http11-p)
          (setf (response-param "ETag") (etag-file body))))
      (unless (response-param "Last-Modified")
        (if (and (pathnamep body) (fad:file-exists-p body) 
                   (not (fad:directory-pathname-p body)))
          (setf (response-param "Last-Modified") 
		(date:universal-time-to-http-date (file-write-date body)))
	  (setf (response-param "Last-Modified") 
		(date:universal-time-to-http-date (get-universal-time)))))
      (unless (response-param "Content-Length")
	(cond 
	  ((pathnamep body) (setf (response-param "Content-Length")
				  (file-size body)))
	  ((streamp body) (setf (response-param "Transfer-Encoding") "chunked"))
	  ((stringp body) (let ((body-bytes (babel:string-to-octets body :encoding :utf-8)))
			    (setf (http-response-body-content response)
				  (flexi-streams:make-in-memory-input-stream body-bytes)
				  (response-param  "Content-Length") 
				  (format nil "~d" (length  body-bytes)))))))
      (validate-header-fields *kuma-request* *kuma-response*))))

(defun validate-range (pathname if-range)
  (let ((http-date (date:parse-time if-range)))
    (or (and http-date 
	     (<= (iolib.syscalls:stat-mtime (iolib.syscalls:stat (namestring pathname)))
		 http-date))
	(string= if-range (etag-file pathname)))))

(defun validate-unmodified-since (last-modified date)
  (<= last-modified
      date))

(defun validate-match (etag if-match)
  (or (string= "*" if-match) 
      (string= etag if-match)))

(defmethod validate-header-fields ((request http-request) (response http-response))
  (let* ((*kuma-response* response)
	 (*kuma-request* request)
	 (method (http-request-method request))
	 (body (http-response-body-content response))	 
	 (if-match (request-param "If-Match"))
	 (if-none-match (request-param "If-None-Match"))
	 (if-modified-since (alexandria:when-let ((str (request-param "If-Modified-Since")))
			      (date:parse-time str)))
	 (if-unmodified-since (alexandria:when-let ((str (request-param "If-Unmodified-Since")))
				(date:parse-time str)))
	 (if-range (request-param "If-Range"))
	 (range (%parse-range (request-param "Range")))
	 (no-cache (or (cl-ppcre:scan "no-cache" (request-param "Pragma"))
		       (cl-ppcre:scan "no-cache" (request-param "Cache-Control"))))
	 (etag (response-param "ETag"))
	 (last-modified (date:parse-time (response-param "Last-Modified")))
	 (body-range-p nil))
    (when (not no-cache)
      (if (pathnamep body)
	  (if range
	    (progn
	      (when if-range 
		(setf body-range-p (validate-range body if-range)))
	      (when (and if-unmodified-since
			 (not (or if-none-match if-modified-since)))
		(if (validate-unmodified-since last-modified if-unmodified-since)
		    (setf body-range-p t)
		    (progn
		      (clear-response-params)		      
		      (setf (status-line) +http-precondition-failed+
			    (http-response-body-content response) nil))))
	      (when (and if-match
			 (not (or if-none-match if-modified-since)))
		(if (validate-match etag if-match)
		    (setf body-range-p t)
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-precondition-failed+
			    (http-response-body-content response) nil))))
	      (when (and if-none-match
			 (not (or if-match if-unmodified-since)))
		(if (not (validate-match etag if-none-match))
		    (progn 
		      (unless (or (string-equal "GET" method)
			      (string-equal "HEAD" method))
			(clear-response-params))
		      (setf (status-line) +http-not-modified+
			    (http-response-body-content response) nil))
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-precondition-failed+
			    (http-response-body-content response) nil))))
	      (when (and if-modified-since
			 (not (or if-match if-unmodified-since)))
		(when (validate-unmodified-since last-modified if-modified-since)		  
		  (clear-response-params)
		  (setf (status-line) +http-not-modified+
			(http-response-body-content response) nil)))
	      (when (and body-range-p body)
		(let ((stream (make-instance 'range-stream 
					     :pathname body 
					     :byte-range range)))
		  (setf (status-line) +http-partial-content+
			(response-param "Content-Length") (+ (- (range-stream-end stream)
								(range-stream-begin stream))
							     1)
			(response-param "Content-Range") (format nil "bytes ~d-~d/~d"
								 (range-stream-begin stream)
								 (range-stream-end stream)
								 (range-stream-file-size stream))
			;(response-param  "ETag") nil
			(http-response-body-content response) stream))))
	    (progn
	      (when (and if-unmodified-since
			 (not (or if-none-match if-modified-since)))
		(if (validate-unmodified-since last-modified if-unmodified-since)
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-not-modified+
			    (http-response-body-content response) nil))
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-precondition-failed+
			    (http-response-body-content response) nil))))
	      (when (and if-match
			 (not (or if-none-match if-modified-since)))
		(if (validate-match etag if-match)
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-not-modified+
			    (http-response-body-content response) nil))
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-precondition-failed+
			    (http-response-body-content response) nil))))
	      (when (and if-none-match
			 (not (or if-match if-unmodified-since)))
		(if (not (validate-match etag if-none-match))
		    (progn
		      (unless (or (string-equal "GET" method)
				  (string-equal "HEAD" method))
			(clear-response-params))
		      (setf (status-line) +http-not-modified+
			    (http-response-body-content response) nil))
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-precondition-failed+
			    (http-response-body-content response) nil))))
	      (when (and if-modified-since
			 (not (or if-match if-unmodified-since)))
		(if (validate-unmodified-since last-modified if-modified-since)
		    (progn
		      (clear-response-params)
		      (setf (status-line) +http-not-modified+
			    (http-response-body-content response) nil))
		  (progn
		      (clear-response-params)
		      (setf (status-line) +http-precondition-failed+
			    (http-response-body-content response) nil))))))
	  ))))

(defmethod initialize-instance :after ((reader http-response-reader) &rest initargs)
  (declare (optimize (debug 3)))
  (declare (ignore initargs))
  (with-accessors ((response http-response-reader-response) 
                   (request http-response-reader-request) 
                   (status-line-stream status-line-stream)
                   (header-stream header-stream)
                   (body-stream body-stream))
      reader
    (setf body-stream (let ((body (http-response-body-content response))) 
			(typecase body
			  (string (flexi-streams:make-in-memory-input-stream
				   (babel:string-to-octets body :encoding :utf-8)))
			  (pathname (open body :element-type '(unsigned-byte 8))) ;;todo: use iolib
			  (stream (if (not (response-param "Content-Length")) 
				      (make-instance 'chunked-stream :stream body)
				      body))
			  (t (flexi-streams:make-in-memory-input-stream +crlf+))))
	  header-stream (create-response-header-stream reader)
	  status-line-stream (flexi-streams:make-in-memory-input-stream 
			      (babel:string-to-octets (make-http-line  (status-line response)) 
						      :encoding :ascii)))))

(defmethod create-response-stream ((reader http-response-reader))
  (with-accessors ((status-line-stream status-line-stream)
                   (header-stream header-stream)
                   (body-stream body-stream))
      reader
    (apply #'make-concatenated-stream 
	   (remove-if #'null (list status-line-stream
				   header-stream
				   body-stream)))))

(defmethod closable-close ((reader http-response-reader))
  (with-accessors ((status-line-stream status-line-stream)
                   (header-stream header-stream)
                   (body-stream body-stream)
		   (request http-response-reader-request)
		   (response http-response-reader-response))
      reader
    (loop for (k v) on (http-request-post-parameters request) by #'cddr
       when (listp v) 
       do (alexandria:when-let ((pathname (getf v :pathname)))
	    (when (and (pathnamep pathname) (probe-file pathname))
	      (delete-file pathname))))
    (when request (closable-close request))
    (when response (closable-close response))
    (when status-line-stream (close status-line-stream))
    (when header-stream (close header-stream))
    (when body-stream (close body-stream))))
;; ===================== Internal cache ========================

(defgeneric get-file-from-cache (cache pathname request)
  (:documentation "Returns the file itself, when it doesn't need to be compressed.
When compression is requested, checks the availability in cache, when the resource is not found, it creates one."))
(defclass internal-cache ()
  ())



;;-------------------------------------- Request worker -----------------------;;
(defgeneric content-disposition (form-data-header)
  (:documentation "Returns Content-Disposition of a form-data as a plist like
\(:dispsition \"form-data\" :name \"par1\" :file-name \"file1.txt\"\)"))

(defgeneric content-type (form-data-header)
  (:documentation "Returns (when present) Content-Type of a form-data as a plist like
\(:type \"multipart/mixed\" :boundary \"AaBx12\"\)"))

(defclass form-data-header (header)
  ((content-disposition :reader content-disposition)
   (content-type :reader content-type)
   (content-transfer-encoding :reader content-transfer-encoding)))

(defmethod content-disposition ((header form-data-header))
  (alexandria:when-let ((disposition (header-slot-value header 'content-disposition)))
    (let ((disposition-ls (split-sequence:split-sequence #\; disposition)))
      (append (list :disposition (trim-white-spaces (first disposition-ls)))
	      (loop for item in (rest disposition-ls)
		   for match-list = (multiple-value-bind (match scan)
					(cl-ppcre:scan-to-strings "([^=,\\s]*)=\"?([^\"]*)" item)
				      (when match
					(list (intern (string-upcase (aref scan 0)) :keyword) (aref scan 1))))
		   when match-list
		   collect (first match-list)
		   collect (second match-list))))))

(defmethod content-type ((header form-data-header))
  (let ((disposition (header-slot-value header 'content-type)))
    (when disposition
      (let ((disposition-ls (split-sequence:split-sequence #\; disposition)))
	(append (list :type (trim-white-spaces (first disposition-ls)))
		(loop for item in (rest disposition-ls)
		   for match-list = (multiple-value-bind (match scan)
					(cl-ppcre:scan-to-strings "([^=,\\s]*)=\"?([^\"]*)" item)
				      (when match
					(list (intern (string-upcase (aref scan 0)) :keyword) (aref scan 1))))
		   when match-list
		   collect (first match-list)
		   collect (second match-list)))))))

(defmethod content-transfer-encoding ((header form-data-header))
  (header-slot-value header 'content-transfer-encoding))

(defgeneric worker-read-byte (worker byte))

(defgeneric form-data-read-byte (form-data byte))
(defgeneric form-data-close (form-data request))
(defgeneric content-disposition-disposition (form-data))
(defgeneric content-disposition-name (form-data))
(defgeneric content-disposition-filename (form-data))
(defgeneric content-type-type (form-data))
(defgeneric content-type-charset (form-data))
(defgeneric content-type-boundary (form-data))
(defgeneric parse-form-data-value (form-data value-octets))

(defclass form-data ()
  ((hader :accessor header :initform nil)
   (content-disposition-disposition :accessor content-disposition-disposition)
   (content-disposition-name :accessor content-disposition-name)
   (content-disposition-filename :accessor content-disposition-filename)
   (content-type-type :accessor content-type-type)
   (content-type-charset :accessor content-type-charset)
   (content-type-boundary :accessor content-type-boundary)
   (content-transfer-encoding :accessor content-transfer-encoding)
   (fcontent-pathname :accessor fcontent-pathname :initform nil)
   (fcontent :accessor fcontent :initform nil)
   (fcontent-buffer :accessor fcontent-buffer :initform (make-array 2
								    :element-type '(unsigned-byte 8)
								    :adjustable t))
   (fcontent-pointer :accessor fcontent-pointer :initform 0)
   (worker :accessor form-data-worker :initform nil)
   (read-pointer :accessor read-pointer :initform 0)
   (read-buffer :accessor read-buffer :initform (make-array 65536
							    :element-type '(unsigned-byte 8)
							    :adjustable t))))

(defmethod content-disposition-disposition ((form-data form-data))
  (if (slot-boundp form-data 'content-disposition-disposition)
      (slot-value form-data 'content-disposition-disposition)
      (setf (slot-value form-data 'content-disposition-disposition)
	    (getf (content-disposition (header form-data)) :disposition))))

(defmethod content-disposition-name ((form-data form-data))
  (if (slot-boundp form-data 'content-disposition-name)
      (slot-value form-data 'content-disposition-name)
      (setf (slot-value form-data 'content-disposition-name)
	    (getf (content-disposition (header form-data)) :name))))

(defmethod content-disposition-filename ((form-data form-data))
  (if (slot-boundp form-data 'content-disposition-filename)
      (slot-value form-data 'content-disposition-filename)
      (setf (slot-value form-data 'content-disposition-filename)
	    (getf (content-disposition (header form-data)) :filename))))

(defmethod content-type-type ((form-data form-data))
  (if (slot-boundp form-data 'content-type-type)
      (slot-value form-data 'content-type-type)
      (setf (slot-value form-data 'content-type-type)
	    (or (getf (content-type (header form-data)) :type)
		"text/plain"))))

(defmethod content-type-charset ((form-data form-data))
  (if (slot-boundp form-data 'content-type-charset)
      (slot-value form-data 'content-type-charset)
      (setf (slot-value form-data 'content-type-charset)
	    (or (getf (content-type (header form-data)) :charset)
		(and (string-equal "text/plain" (content-type-type form-data))
		     "utf-8")))))

(defmethod content-type-boundary ((form-data form-data))
  (if (slot-boundp form-data 'content-type-boundary)
      (slot-value form-data 'content-type-boundary)
      (setf (slot-value form-data 'content-type-boundary)
	    (alexandria:when-let ((boundary (getf (content-type (header form-data)) :boundary)))
	      (babel:string-to-octets boundary :encoding :ascii)))))

(defmethod content-transfer-encoding ((form-data form-data))
  (content-transfer-encoding (header form-data)))

(defmethod parse-form-data-value ((form-data form-data) value-octets)
  (if (string-equal "text/plain" (content-type-type form-data))
      (let* ((transfer-encoding (content-transfer-encoding form-data))
	     (charset (string-upcase (content-type-charset form-data)))
	     (value (babel:octets-to-string value-octets 
					    :encoding (alexandria:make-keyword charset))))
	(cond
	  ((and (or (string-equal "ascii" charset)
		    (string-equal "us-ascii" charset)))
	   (cond
	     ((string-equal "quoted-printable" transfer-encoding) (%quoted-decode value :attribute-p nil))
	     ((string-equal "base64" transfer-encoding) (%base64-decode value))
	     (t (parse-rfc1738-value value))))
	  (t 
	   (cond
	     ((string-equal "quoted-printable" transfer-encoding) (%quoted-decode value :attribute-p nil))
	     ((string-equal "base64" transfer-encoding) (%base64-decode value))
	     (t value)))))
      value-octets))

(defmethod form-data-read-byte ((form-data form-data) byte)
  (with-accessors ((pointer read-pointer)
		   (buffer read-buffer)
		   (header header))
      form-data
    (let ((buff-length (length buffer)))
      (when (= pointer buff-length)
	(adjust-array buffer (* buff-length 2)))
      (setf (aref buffer pointer) byte)
      (incf pointer)
      
      (cond
	((and (not header)
	      (> pointer 4)
	      (equalp (subseq buffer (- pointer 4) pointer) +http-header-separator+)) ;;header parser
	 (setf header (make-instance 'form-data-header))
	 (let ((parsed-headers (%parse-headers (make-array pointer
							:element-type '(unsigned-byte 8)
							:displaced-to buffer))))
	   (loop for (k v) on parsed-headers by #'cddr
	      do (setf (header-value header k) v))
	   (setf pointer 0)))
	((and header
	      (alexandria:starts-with-subseq "multipart" (content-type-type form-data)))
	 (with-accessors ((form-data-worker form-data-worker))
	     form-data
	   (unless form-data-worker 
	     (setf form-data-worker 
		   (make-instance 'multipart-body-woker
				  :multipart-disposition-name (content-disposition-name form-data))))
	   (worker-read-byte form-data-worker byte)))
	((and header 
	      (string-equal "form-data" (content-disposition-disposition form-data))
	      (content-disposition-filename form-data)) ;; content-disposition is a file
	 (progn	   
	   (if (< (fcontent-pointer form-data) 2)
	       (progn
		 (setf (aref (fcontent-buffer form-data) (fcontent-pointer form-data)) byte)
		 (incf (fcontent-pointer form-data)))
	       (let ((out-byte (aref (fcontent-buffer form-data) 0)))
		 (setf (fcontent-buffer form-data)
		       (append-to-array-and-shift (fcontent-buffer form-data) byte))
		 (unless (fcontent form-data)
		   (let ((pathname (merge-pathnames (make-pathname :name (symbol-name (gensym)))
						    *tmp-path*)))
		     (setf (fcontent-pathname form-data) pathname
			   (fcontent form-data) (open pathname
						      :direction :output
						      :element-type '(unsigned-byte 8)
						      :if-exists :supersede
						      :if-does-not-exist :create))))
		 (write-byte out-byte (fcontent form-data))
		 )))))
      )))

(defmethod form-data-close ((form-data form-data) (request http-request))
  (cond 
    ((and (not (content-disposition-filename form-data))
	  (string-equal "form-data" (content-disposition-disposition form-data)))
     (alexandria:when-let ((value (parse-form-data-value form-data (subseq (read-buffer form-data)
									   0
									   (- (read-pointer form-data) 2)))))
       (add-post-parameter request
			   (content-disposition-name form-data)
			   value)))
    ;;; add multipart
    ((content-disposition-filename form-data)
     (progn
       (alexandria:when-let ((fcontent (fcontent form-data)))
	 (close fcontent)
	 (add-post-parameter request
			     (content-disposition-name form-data)
			     (alexandria:when-let ((pathname (fcontent-pathname form-data)))
			       (list :filename (content-disposition-filename form-data)
				     :pathname pathname))))))))

(defgeneric boundary-reachedp (worker)
  (:documentation "Returns a pair of values, first value is T if boundary is met, second value is T
if it is the last boundary")) 

(defclass multipart-body-woker ()
  ((boundary :reader http-boundary :initarg :boundary)
   (http-request :reader http-request :initarg :http-request)
   (read-pointer :accessor read-pointer :initform 0)
   (read-buffer-length :reader read-buffer-length)
   (read-buffer :accessor read-buffer)
   (multipart-disposition-name :accessor multipart-disposition-name :initarg :multipart-disposition-name)
   (last-boundary-p :accessor last-boundary-p :initform nil)
   (form-data :accessor form-data :initform nil))
  (:default-initargs :multipart-disposition-name nil))

(defmethod initialize-instance :after ((worker multipart-body-woker) &rest initargs)
  (declare (ignore initargs))
  (let ((length (+ (length (http-boundary worker)) 4))) ;; "--" + "--|CRLF"
    (setf (slot-value worker 'read-buffer-length) length
	  (read-buffer worker) (make-array length
					   :adjustable t
					   :element-type '(unsigned-byte 8) 
					   :initial-element 0))))

(defmethod boundary-reachedp ((worker multipart-body-woker))
  (let ((read-buffer (read-buffer worker))
	(pointer (read-pointer worker)))
    (and (= (read-pointer worker) (read-buffer-length worker))
	 (if (equalp (http-boundary worker)  
		     (make-array (length (http-boundary worker)) 
				 :element-type '(unsigned-byte 8)
				 :displaced-to read-buffer
				 :displaced-index-offset 2))
	     (let ((last-two-bytes (make-array 2
				    :element-type '(unsigned-byte 8)
				    :displaced-to read-buffer
				    :displaced-index-offset (- pointer 2))))
	       (cond
		 ((equalp #(13 10) #|CR LF|#
			  last-two-bytes) 		
		  (values t nil))
		 ((equalp #(45 45) #|--|#
			  last-two-bytes)		
		  ;;(setf (slot-value (http-request worker) 'body-entity-read-p) t)
		  (values t t))
		 (t (progn 
		      ;;(iolib.syscalls:syslog iolib.syscalls:log-alert "boundary end ~a" last-two-bytes)
		      (error 'http-bad-request-condition)))))
	     (values nil nil)))))

(defmethod worker-read-byte ((worker multipart-body-woker) byte)
  (cond 
    ((last-boundary-p worker)
     ;;; receiving last CRLF
     (if (< (read-pointer worker) 2)
	 (progn
	   (incf (read-pointer worker))
	   t)
	 nil))
    ((< (read-pointer worker) (read-buffer-length worker))
     ;; when buffer is NOT filled
     (progn
       (setf (aref (read-buffer worker) (read-pointer worker)) byte)
       (incf (read-pointer worker))))
    (t
     ;; when buffer IS filled we can proceed in evaluating it
     (multiple-value-bind (boundary-met-p last-boundary-p)
	 (boundary-reachedp worker)
       (cond	 
	 (last-boundary-p 
	  (progn (alexandria:when-let ((form-data (form-data worker)))
		   (form-data-close form-data (http-request worker)))
		 (setf (last-boundary-p worker) t)
		 t))
	 ((and boundary-met-p (not last-boundary-p)) 
	  (progn
	    (alexandria:when-let ((form-data (form-data worker)))
	      (form-data-close form-data (http-request worker)))
	    (setf (form-data worker) (make-instance 'form-data))
	    (alexandria:when-let ((name (multipart-disposition-name worker)))
	      (setf (content-disposition-name (form-data worker)) name))
	    t)))       
       (let ((out-byte (aref (read-buffer worker) 0))) ;;; getting first byte of the buffer	    
	 ;; here we shift left the buffer and add byte read to last pos in the buffer
	 (setf (read-buffer worker) 
	       (append-to-array-and-shift (read-buffer worker) byte)) 	   
	 (alexandria:when-let ((form-data (form-data worker)))
	   (form-data-read-byte form-data out-byte))
	 t)))))

;; ------------------------ Response worker ------------------------- ;;


(defclass response-handler () 
  ((condition-lambda :accessor response-handler-condition :initarg :condition-lambda)
   (handler-function :accessor response-handler-function :initarg :handler-function))
  (:default-initargs 
   :condition-lambda (lambda () nil)
   :handler-function (lambda () nil)))


;; --------   default errors handler--------------
(defclass response-error-handler (response-handler) 
  ((template :reader handler-template :initarg :template))
  (:default-initargs :template (arnesi:read-string-from-file 
				(make-pathname :directory (append *kuma-src-dir* (list "resources")) 
					       :name "http-error-tpl" 
					       :type "html")
				:external-format :utf-8)))

(defmethod response-handler-condition ((handler response-error-handler))
  t)

(defmethod response-handler-function ((handler response-error-handler))
  (lambda ()
    (let* ((result "")
	   (status (rest (http-response-status-line *kuma-response*)))
	   (error-code (first status)))
      (if (= error-code (first +http-not-modified+))
	  nil
	  (with-accessors ((connection-response-reader connection-response-reader))
	      *kuma-connection*
	    (when connection-response-reader 
	      (close connection-response-reader)
	      (setf connection-response-reader nil))
	    (let* ((error-code (first status))
		   (reason (second status))
		   (request-uri (http-request-uri *kuma-request*))
		   (server-name (kuma-server-name *kuma-server*))
		   (template (handler-template handler)))
	      (setf result (format nil
				   template
				   error-code
				   error-code
				   reason
				   request-uri
				   server-name)))))
      result
      (flexi-streams:make-in-memory-input-stream (babel:string-to-octets result :encoding :utf-8)))))
