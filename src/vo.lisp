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

(defgeneric header-value (header key))

(defgeneric setf-header-value (header key value))

(defgeneric header-slot-value (header slot))

(defgeneric header-slot-value-as-time (header slot))

(defgeneric header-slot-value-as-number (header slot))

(defgeneric header-slot-multi-value (header slot))

(defclass header ()
  ((headers :reader header-headers :initarg :headers))
  (:default-initargs :headers (make-hash-table :test #'equalp)))

(defmethod header-value ((header header) key)
  (gethash (or (and (symbolp key) (symbol-name key)) key)
           (header-headers header)))

(defmethod setf-header-value ((header header) key value)
  (if value
      (setf (gethash (or (and (symbolp key) (symbol-name key)) key)
                     (header-headers header))
            value)
      (remhash key (header-headers header))))



(defun get-request-param (key &optional (request *kuma-request*))
  (header-value request key))

(defmethod header-multi-value ((header header) key)
  (%parse-multi-value (getf (header-headers header) key)))

(defmethod header-slot-value ((header header) slot)
  (if (slot-boundp header slot)
      (slot-value header slot)
      (setf (slot-value header slot)
            (header-value header (intern slot :keyword)))))

 (defmethod header-slot-value-as-time ((header header) slot)
  (if (slot-boundp header slot)
      (slot-value header slot)      
      (setf (slot-value header slot)
            (let ((time-string (header-value header (intern slot :keyword))))
              (and time-string (date:parse-time time-string)))))) 

(defmethod header-slot-value-as-number ((header header) slot)
  (if (slot-boundp header slot)
      (slot-value header slot)      
      (setf (slot-value header slot)
            (let ((number-string (header-value header (intern slot :keyword))))
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

(defgeneric header-accept-encoding (header encoding))

(defclass request-header (general-header entity-header)
  ((accept :accessor header-accept)
   (accept-charset :accessor header-accept-charset :initform nil)
   (accept-encoding)
   (accept-language :accessor header-accept-language :initform nil)
   (authorization :accessor header-authorization :initform nil)
   (expect :accessor header-expect :initform nil)
   (from :accessor header-from :initform nil)
   (host :accessor header-host :initform nil)
   (if-match :accessor header-if-match :initform nil)
   (if-modified-since :accessor header-if-modified-since :initform nil)
   (if-none-match :accessor header-if-none-match :initform nil)
   (if-range :accessor header-if-range :initform nil)
   (if-unmodified-since :accessor header-if-unmodified-since :initform nil)
   (max-forwards :accessor header-max-forwards :initform nil)
   (proxy-authorization :accessor header-proxy-authorization :initform nil)
   (range :accessor header-range :initform nil)
   (referer :accessor header-referer :initform nil)
   (te :accessor header-te :initform nil)
   (user-agent :accessor header-user-agent :initform nil))
  (:default-initargs :headers nil))

(defmethod header-accept ((header request-header))
  (header-slot-multi-value header 'accept))

(defmethod header-accept-encoding ((header request-header) encoding)
  (unless (slot-boundp header 'accept-encoding)
    (setf (slot-value header 'accept-encoding) 
          (header-slot-multi-value header 'accept-encoding)))
  (or (string-equal encoding "identity")
      (member-if (lambda (item) (string-equal encoding item))
                 (slot-value header 'accept-encoding))))

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

(defclass http-request ()
  ((requset-line :reader http-request-request-line :initform nil)
   (header :reader http-request-header :initform (make-instance 'request-header))
   (body-entity :reader http-request-body-entity :initform nil)
   (get-parameters :reader http-request-get-parameters :initform nil)
   (post-parameters :reader http-request-post-parameters :initform nil)
   (done-p :accessor http-request-done-p :initform nil)))

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

(defclass http-response ()
  ((status-line :accessor http-response-status-line :initarg :status-line)
   (response-header :reader http-response-header :initform (make-instance 'response-header))
   (body-content :accessor http-response-body-content :initarg :body-content))
  (:default-initargs :body-content nil :status-line (cons "HTTP/1.1" +http-ok+)))

(defun set-response-param (key value &optional (response *kuma-response*))
  (setf-header-value (http-response-header response) key value))

(defun get-response-param (key &optional (response *kuma-response*))
  (header-value (http-response-header response) key))

(defun (setf status-line) (code)
  (setf (http-response-status-line *kuma-response*) 
        (cons "HTTP/1.1" (or (and (listp code) code)
                             (list code (getf +http-http-results+ code))))))

(defun status-line (&optional (response *kuma-response*))
  (format nil "~{~a~^ ~}" (http-response-status-line response)))


(defclass connection ()
  ((client :reader connection-client :initarg :client)
   (who :reader connection-who :initarg :who)
   (port :reader connection-port :initarg :port)
   (read-buffer :accessor connection-read-buffer :initform nil)
   (write-buffer :accessor connection-write-buffer :initform nil)
   (request :reader connection-request :initform (make-instance 'http-request))
   (request-pipeline :accessor connection-request-pipeline :initform (make-instance 'arnesi:queue))
   (response :accessor connection-response :initform nil)
   (worker-lock :accessor connection-worker-lock :initform (bt:make-lock))))

(defun create-easy-response-handler (&key uri function)
  (lambda ()
    (when (and uri
               (cl-ppcre:scan (cl-ppcre:create-scanner uri :case-insensitive-mode t)
                              (http-request-location *kuma-request*)))
      function)))

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

(defgeneric complete-response-header (response request))

(defgeneric create-response-header-stream (reader))
(defgeneric create-response-body-reader (reader))
(defgeneric create-response-stream (reader))

(defclass http-response-reader ()
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
               (flexi-streams:make-in-memory-input-stream
                (babel:string-to-octets (make-http-line (format nil "~{~a~}"
                                                                header-strings)))))))))

(defmethod complete-response-header ((response http-response) (request http-request))
  (let ((*kuma-request* request)
        (*kuma-response* response)
        (body (http-response-body-content response))
        (client-http11-p (string-equal "HTTP/1.1" (http-request-http-version request))))
    (when body
      (when (and (pathnamep body) (not (fad:file-exists-p body)))
	(error 'http-not-found-condition))
      (when (and (pathnamep body) (fad:directory-pathname-p body))
	(error 'http-forbidden-condition))
      (unless (get-response-param "Content-Type")
        (when (and (pathnamep body) (fad:file-exists-p body) (not (fad:directory-pathname-p body)))
          (let ((content-type (or (get-mime body) "application/octet-stream")))
            (set-response-param "Content-Type" content-type))))

      (unless (get-response-param "ETag")
        (when (and (pathnamep body) (fad:file-exists-p body) 
                   (not (fad:directory-pathname-p body)) client-http11-p)
          (set-response-param "ETag" (etag-file body))))

      (unless (get-response-param "Last-Modified")
        (when (and (pathnamep body) (fad:file-exists-p body) 
                   (not (fad:directory-pathname-p body)))
          (set-response-param "Last-Modified" 
                              (date:universal-time-to-http-date (file-write-date body)))))
      
      (when (and (pathnamep body) (not (get-response-param "Content-Length")))
	(set-response-param "Content-Length" (format nil "~a" 
						     (iolib.syscalls:stat-size 
						      (iolib.syscalls:stat 
						       (namestring body)))))))))

(defmethod initialize-instance :after ((reader http-response-reader) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((response http-response-reader-response) 
                   (request http-response-reader-request) 
                   (status-line-stream status-line-stream)
                   (header-stream header-stream)
                   (body-stream body-stream))
      reader
    (let ((body (http-response-body-content response)))
      (setf status-line-stream (flexi-streams:make-in-memory-input-stream 
				(babel:string-to-octets (make-http-line  (status-line response)) 
							:encoding :ascii))
	    header-stream (create-response-header-stream reader)
	    body-stream (typecase body
			  (string (flexi-streams:make-in-memory-input-stream
				   (babel:string-to-octets body :encoding :utf8)))
			  (pathname (open body :element-type '(unsigned-byte 8)))
			  (t body))))))

(defmethod create-response-stream ((reader http-response-reader))
  (with-accessors ((status-line-stream status-line-stream)
                   (header-stream header-stream)
                   (body-stream body-stream))
      reader    
    (apply #'make-concatenated-stream 
	   (remove-if #'null (list status-line-stream
				   header-stream
				   body-stream)))))

(defmethod close ((reader http-response-reader) &key abort)
  (declare (ignore abort))
  (with-accessors ((status-line-stream status-line-stream)
                   (header-stream header-stream)
                   (body-stream body-stream))
      reader
    (when status-line-stream (close status-line-stream))
    (when header-stream (close header-stream))
    (when body-stream (close body-stream))))
;; ===================== Internal cache ========================

(defgeneric get-file-from-cache (cache pathname request)
  (:documentation "Returns the file itself, when it doesn't need to be compressed.
When compression is requested, checks the availability in cache, when the resource is not found, it creates one."))
(defclass internal-cache ()
  ())