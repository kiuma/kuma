;;; -*- lisp  -*-
;;; $Header: src/kuma.lisp $

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



;;; This file is part of fast-connector. See LICENSE for licence terms.

(in-package :kuma)

;;; setup a multiplexer
(defvar *removeme*)
(defvar *max-bytes* (* 1024 64))
(defconstant +read-timeout+ 10)
(defconstant +write-timeout+ 10)

(defvar *read-timeout* +read-timeout+)
(defvar *write-timeout* +write-timeout+)

(defvar *worker-function*)

;;; ==================================================================================
(defvar *kuma-server* nil)
;(shadow 'funcall)
;(defgeneric funcall (f &rest args) (:method (f &rest args) (apply 'cl:funcall f args)))

(defgeneric kuma-server-error-handler (server))
(defgeneric disconnector-lock (server))
(defgeneric (setf kuma-server-error-handler) (error-handler server))

(defgeneric kuma-server-prepare-response (listener connection request fd socket))
(defgeneric kuma-server-process-response (listener connection))

(defgeneric kuma-listener-run (listener))
(defgeneric socket-listener-run-helper (listener))

(defgeneric make-socket-listener-handler (listener socket))
(defgeneric make-socket-listener-read-some-bytes (listener))
(defgeneric make-socket-listener-write-some-bytes (listener))

(defgeneric listener-disconnect (listener connection fd socket &rest events))


(defun make-request-status-line-worker ()
  (lambda (listener connection buffer buffer-length)
    (declare (ignore listener))
    (let* ((http-request (connection-request connection)))
      (if (not (http-request-request-line http-request))
	  (progn
	    (when (and (> buffer-length 2)
		       (equalp (subseq buffer (- buffer-length 2) buffer-length)
			       +crlf+))
	      (setf (http-request-request-line http-request) (%parse-http-request-line
							      (subseq buffer 0 (- buffer-length 2)))
		    (connection-read-buffer-pointer connection) 0))
	    t)
	  nil))))

(defun make-request-header-worker ()
  (lambda (listener connection buffer buffer-length)
    (let* ((http-request (connection-request connection))
	   (header (http-request-header http-request))
	   (header-headers (header-headers header))
	   (fd (socket-os-fd (connection-client connection))))
      (if (and (http-request-request-line http-request) (not header-headers))
	  (progn
	    (when (and (> buffer-length 4)
		       (equalp (subseq buffer (- buffer-length 4) buffer-length)
			       +http-header-separator+))
	      (let* ((socket (connection-client connection))
		     (request (connection-request connection))
		     (request-header (http-request-header request)))
		(setf (headers-buffer header) (subseq buffer 0 (- buffer-length 4))
		      (header-headers header) (make-hash-table :test #'equalp))
		(loop for (k v) on (%parse-headers (make-array buffer-length
							       :element-type '(unsigned-byte 8)
							       :displaced-to buffer)) by #'cddr
		   do (setf (header-value header k) v))
		(setf (slot-value http-request 'body-boundary)
		      (alexandria:when-let
			  ((b (boundary (header-content-type
					 (http-request-header request)))))
			(babel:string-to-octets b :encoding :utf-8))
		      (connection-read-buffer-pointer connection) 0)
		(alexandria:when-let ((expect (header-expect request-header)))
		  (if (string-equal expect "100-continue")
		      (let ((continue
			     (babel:string-to-octets (make-http-line
						      (make-http-line
						       (format nil "HTTP/1.1 ~{~a~^ ~}" +http-continue+)))
						     :encoding :utf-8)))
			(send-to socket
				 continue
				 :end (length continue))
			(setf (header-expect request-header) nil))))
		(kuma-server-prepare-response listener connection request fd socket)
		(unless (header-content-type (http-request-header request))
		  ;;; process response
		  )))
	    t)
	  (progn
	    ;;(iolib.syscalls:syslog iolib.syscalls:log-alert "byte read ~a" "header-worker FINISH")
	    nil)))))

(defun make-request-urlencoded-body-worker ()
  (lambda (listener connection buffer buffer-length)
    (declare (ignore listener))
    (let* ((http-request (connection-request connection))
	   (header (http-request-header http-request))
	   (content-length (header-content-length header)))
      (when (and (string-equal (header-content-type header) "application/x-www-form-urlencoded")
	       (not (http-request-body-entity-read-p (connection-request connection))))
	  (if (>= (connection-read-buffer-pointer connection) content-length)
	      (progn
		(setf (slot-value http-request 'post-parameters) (parse-rfc1738 (babel:octets-to-string
										 (make-array buffer-length
											     :element-type '(unsigned-byte 8)
											     :displaced-to buffer)
										 :encoding :utf-8))
		      (slot-value http-request 'body-entity-read-p) t)
		nil)
	      t)))))


;;;; imcomplete
(defun make-request-multipart-body-worker ()
  (lambda (listener connection buffer buffer-length)
    (declare (ignore buffer-length listener))
    (let* ((http-request (connection-request connection))
	   (boundary (http-request-body-boundary http-request))
	   (worker (and boundary
			(or (http-request-worker http-request)
			    (setf (http-request-worker http-request)
				  (make-instance 'multipart-body-woker
						 :http-request http-request
						 :boundary boundary))))))
      (if (and boundary
	       (not (http-request-body-entity-read-p http-request)))
	  (progn
	    ;;(iolib.syscalls:syslog iolib.syscalls:log-alert "byte read ~a" "multipart-body-worker")
	    (setf (connection-read-buffer-pointer connection) 0)
	    (worker-read-byte worker (aref buffer 0)))
	  (progn
	    ;;(iolib.syscalls:syslog iolib.syscalls:log-alert "byte read ~a" "multipart-body-worker FINISH")
	    nil)))))

(defclass worker-callable (callable)
  ((connection :accessor callable-connection :initarg :connection)
   (request :accessor callable-request :initarg :request)
   (response :accessor callable-response :initarg :response)
   (listener :accessor callable-listener :initarg :listener)
   (server :accessor callable-server :initarg :server)
   (socket :accessor callable-socket :initarg :socket)
   (fd :accessor callable-fd :initarg :fd)))

(defmethod callable-call ((callable worker-callable))
  (let* ((*kuma-connection* (callable-connection callable))
	 (*kuma-request* (callable-request callable))
	 (*kuma-response* (callable-response callable))
	 (*kuma-server* (callable-server callable))
	 (server-pool (kuma-server-threads (callable-server callable)))
	 (worker-lock (connection-worker-lock *kuma-connection*))
	 (callable-lock (connection-callable-lock *kuma-connection*))
	 (error-handler (kuma-server-error-handler *kuma-server*)))
    (bt:with-lock-held (worker-lock)
      (let ((listener (callable-listener callable))
	    (client-fd (callable-fd callable))
	    (socket (callable-socket callable)))
	(handler-case
	    (let ((call-result (funcall (callable-handler-func callable))))
	      (setf (http-response-body-content *kuma-response*) call-result))
	  (http-error (e)
	    (setf (status-line) (list (error-code e) (error-description e)))
	    (let ((call-result (funcall (response-handler-function error-handler))))
	      (setf (http-response-body-content *kuma-response*) call-result))
	    (listener-disconnect listener *kuma-connection* client-fd socket :read))
	  (error (e)
	    (declare (ignore e))
	    (setf (status-line) +http-internal-server-error+)
	    (let ((call-result (funcall (response-handler-function error-handler))))
	      (setf (http-response-body-content *kuma-response*) call-result))
	    (listener-disconnect listener *kuma-connection* client-fd socket :read)))    
	(bt:with-lock-held (callable-lock)
	  (arnesi:enqueue (connection-request-pipeline *kuma-connection*)
			  (make-instance 'http-response-reader
					 :request *kuma-request*
					 :response *kuma-response*))
	  (unless (io-handler-writer-p *kuma-connection*)
	    (setf (io-handler-writer-p *kuma-connection*) t)
	    (set-io-handler (kuma-event-base listener)
			    client-fd
			    :write
			    (http-listener-write-some-bytes listener)))
	  (alexandria:when-let ((newcallable (arnesi:dequeue (connection-worker-pipeline *kuma-connection*))))
	    (add-to-pool server-pool newcallable)))
	(funcall (http-listener-write-some-bytes listener)
		 client-fd 
		 :write nil)))))

(defclass kuma-listener () 
  ((server :reader kuma-server :initarg :server)))

(defclass socket-listener (kuma-listener)
  ((open-connections :accessor kuma-open-connections :initform (make-hash-table :test #'equalp))
   (max-backlog :reader socket-listener-max-backlog :initarg :max-backlog)
   (event-base :accessor kuma-event-base :initform (make-instance 'event-base)))
  (:default-initargs :max-backlog *default-backlog-size*))

(defmethod initialize-instance :after ((listener socket-listener) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value listener 'read-some-bytes) (make-socket-listener-read-some-bytes listener)
	(slot-value listener 'write-some-bytes) (make-socket-listener-write-some-bytes listener)))

(defclass http-listener (socket-listener)
  ((listener-event-base :accessor kuma-local-event-base :initform nil)
   (port :accessor http-listener-port :initarg :port)
   (bind-address :accessor http-listener-bind-address :initarg :bind-address)
   (read-some-bytes :reader http-listener-read-some-bytes)
   (write-some-bytes :reader http-listener-write-some-bytes)
   (reader-workers :accessor http-listener-reader-workers)
   (local-socket :reader listener-local-socket :initarg :local-socket)
   (write-buffer :accessor http-listener-write-buffer 
		 :initform (make-array *default-buffer-size* :element-type '(unsigned-byte 8))))
  (:default-initargs :bind-address +ipv4-unspecified+))

#|
(defclass http-response-listener (socket-listener)
  ((local-socket :reader listener-local-socket :initarg :local-socket)))
|#
(defmethod initialize-instance :after ((listener http-listener) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value listener 'reader-workers) (list
					       (make-request-status-line-worker)
					       (make-request-header-worker)
					       (make-request-urlencoded-body-worker)
					       (make-request-multipart-body-worker))))

(defmethod kuma-listener-run ((listener socket-listener))
  (unwind-protect
       (handler-case
           (progn
             (socket-listener-run-helper listener))
         ;; handle some common signals
         (socket-address-in-use-error ()
           (format t "Bind: Address already in use, forget :reuse-addr t?")))

    ;; Cleanup form for uw-p
    ;; Close all open connections to the clients, if any. We do this
    ;; because when the server goes away we want the clients to know
    ;; immediately. Sockets are not memory, and can't just be garbage
    ;; collected whenever. They have to be eagerly closed.
    (maphash
     #'(lambda (k v)
         (format t "Force closing a client connection to ~A~%" k)
         (close v :abort t))
     (kuma-open-connections listener))

    ;; and clean up the event-base too!
    (when (kuma-event-base listener)
      (close (kuma-event-base listener)))
    (format t "Server Exited.~%")
    (finish-output)))

(defmethod socket-listener-run-helper ((listener http-listener))
  (let ((bind-address (http-listener-bind-address listener))
        (port (http-listener-port listener))
        (max-backlog (socket-listener-max-backlog listener))
        (server (make-socket :connect :passive
                             :address-family :internet
                             :type :stream
                             :ipv6 nil
                             :external-format '(:utf-8 :eol-style :crlf))))
    (unwind-protect
         (progn
           (bind-address server bind-address :port port :reuse-address t)
           (listen-on server :backlog max-backlog)
           (set-io-handler  (kuma-event-base listener)
                            (socket-os-fd server)
                            :read
                            (make-socket-listener-handler listener server))
           (handler-case ;; TODO close request & response
               (event-dispatch (kuma-event-base listener))
             (socket-connection-reset-error ()
               (format t "Unexpected connection reset by peer!~%"))
             (hangup () (format t "Unexpected hangup!~%"))
             (end-of-file () (format t "Unexpected end of file!~%"))))
      (close server))))


(defmethod make-socket-listener-handler ((listener http-listener) socket)
  (lambda (fd event exception)
    (declare (ignore event exception))
    (format t "listening fd:~a~%" fd)
    ;; do a blocking accept, returning nil if no socket
    (let* ((client (accept-connection socket :wait t))
           (client-fd (and client (socket-os-fd client))))
      (when client
        (format t "listening client-fd:~a~%" client-fd)
        (multiple-value-bind (who port)
            (remote-name client)
          (format t "Accepted a client from ~A:~A~%" who port)

          ;; save the client connection in case we need to close it later.
          (setf (gethash client-fd (kuma-open-connections listener))
                (make-instance 'connection
                               :client client
                               :who who
                               :port port))
          ;; ex-0e

          ;; ex-1b
          ;; We make an io-buffer, which takes care of reading from the
          ;; socket and echoing the information it read back onto the
          ;; socket.  The buffer takes care of this with two internal
          ;; handlers, a read handler and a write handler.
          (format t "Monitoring FD ~a for event READ~%" client-fd)
          (set-io-handler (kuma-event-base listener)
                          client-fd
                          :read
                          (http-listener-read-some-bytes listener)))))))

(defmethod make-socket-listener-write-some-bytes ((listener http-listener))
  (lambda (fd event exception)
    (declare (ignore event exception))
    (let* ((*kuma-connection* (gethash fd (kuma-open-connections listener)))
	   (*kuma-server* (kuma-server listener))
	   (seq-buffer (http-listener-write-buffer listener))
	   (callable-lock (connection-callable-lock *kuma-connection*)))
      (bt:with-lock-held (callable-lock)
	(with-accessors ((who connection-who)
			 (port connection-port)
			 (socket connection-client)
			 (request connection-request)
			 (pipeline connection-request-pipeline)
			 (write-buffer connection-write-buffer)
			 (response-reader connection-response-reader))
	    *kuma-connection*
	  (unless response-reader
	    (setf response-reader (arnesi:dequeue pipeline))
	    (when response-reader
	      (setf write-buffer (create-response-stream response-reader))))
	  (if (and response-reader write-buffer)
	      (progn
		(let ((bytes-read (read-sequence seq-buffer write-buffer)))	      
		  (if (> bytes-read 0)
		      (handler-case 
			  (progn 
			    (send-to socket
				     seq-buffer
				     :end bytes-read))
			(socket-connection-reset-error ()
			  ;; If for somer eaon the client reset the network connection,
			  ;; we'll get this signal.
			  (closable-close response-reader)
			  (setf response-reader nil)
			  (listener-disconnect listener *kuma-connection* fd socket :close))
			(isys:ewouldblock ()
			  ;; Sometimes this happens on a write even though it
			  ;; might have been marked as ready. Also we might have
			  ;; asked to write on an unknown status socket. Ignore
			  ;; it and we will try again later.
			  (format t "write-some-bytes: ewouldblock~%")
			  nil)
			(isys:epipe ()
			  ;; In this server, if the client doesn't accept data,
			  ;; it also means it will never send us data again. So
			  ;; close the connection for good.
			  (closable-close response-reader)
			  (setf response-reader nil)
			  (listener-disconnect listener *kuma-connection* fd socket :close))
			(end-of-file ()
			  (closable-close response-reader)
			  (setf response-reader nil)
			  (listener-disconnect listener *kuma-connection* fd socket :close)))
		      (progn
			(closable-close response-reader)
			(setf response-reader nil)))))
	      (progn
		(when response-reader
		  (closable-close response-reader)
		  (setf response-reader nil))
		(if (io-handler-reader-p *kuma-connection*)		    
		    (listener-disconnect listener *kuma-connection* fd socket :write)
		    (listener-disconnect listener *kuma-connection* fd socket :close)))))))))

(defmethod make-socket-listener-read-some-bytes ((listener http-listener))
  (let ((read-buf (make-array *max-bytes* :element-type 'unsigned-byte)))
    (lambda (fd event exception)
      (declare (ignore event exception))
      (let* ((*kuma-server* (kuma-server listener))
	     (connection (gethash fd (kuma-open-connections listener)))
	     (http-request (connection-request connection))
	     (who (connection-who connection))
	     (port (connection-port connection))
	     (socket (connection-client connection)))
	(handler-case 
	    (multiple-value-bind (buf bytes-read)
		(receive-from socket
			      :buffer read-buf
			      :start 0
			      :end *max-bytes*)
	      (declare (ignore buf))
	      
	      ;; Unlike read-ing from a stream, receive-from
	      ;; returns zero on an end-of-file read, so we turn
	      ;; around and signal that condition so our
	      ;; handler-case can deal with it properly like our
	      ;; other examples.
	      (if (> bytes-read 0)
		;(setf *removeme* (babel:octets-to-string (subseq read-buf 0 bytes-read) :encoding :utf-8))
		(progn
		  (setf (connection-last-access connection) (get-universal-time))
		  (dotimes (i bytes-read)
		    (incf (connection-read-buffer-pointer connection))
		    (when (> (connection-read-buffer-pointer connection)
			     (length (connection-read-buffer connection)))
		      (adjust-array (connection-read-buffer connection)
				    (* (length (connection-read-buffer connection)) 2)))
		    (setf (aref (connection-read-buffer connection)
				(- (connection-read-buffer-pointer connection) 1))
			  (aref read-buf i))
		    (let ((current-buffer (connection-read-buffer connection))
			  (current-buffer-length (connection-read-buffer-pointer connection)))

		      (let* ((worker-run (loop for worker in (http-listener-reader-workers listener)
					    when (funcall worker 
							  listener 
							  connection 
							  current-buffer 
							  current-buffer-length)
					    return t))
			     (lock (disconnector-lock (kuma-server listener)))
			     (io-handler-reader-p t))
			(bt:with-lock-held (lock)
			  (setf io-handler-reader-p (io-handler-reader-p connection)))
			(when (or (not worker-run)
				  (not io-handler-reader-p)
				  (and (header-headers (http-request-header http-request))
				       (not (header-content-type (http-request-header http-request)))))
			  (format t "~%POST parameters: ~a~%" (http-request-post-parameters
							       http-request))
			  (format t "~%GET parameters: ~a~%" (http-request-get-parameters
							      http-request))

			  (setf (connection-read-buffer-pointer connection) 0
				(connection-request connection) (make-instance 'http-request))
			  (kuma-server-process-response listener connection))))))		
		(progn
		  (listener-disconnect listener *kuma-connection* fd socket :close))))
	  (socket-connection-reset-error ()
	    ;; Handle the client sending a reset.
	    (let* ()
	      (format t "Client ~A:~A: connection reset by peer.~%" who port)
	      (closable-close http-request)
	      (listener-disconnect listener *kuma-connection* fd socket :close)))
	  (end-of-file ()
	    (format t "read eof~%")
	    (closable-close http-request)
	    (listener-disconnect listener *kuma-connection* fd socket :close)))))))

(defmethod kuma-server-prepare-response ((listener http-listener) connection request fd socket)
  (let ((server (kuma-server listener)))
    (with-accessors ((handlers kuma-server-handlers)
		     (error-handler kuma-server-error-handler)
		     (server-pool kuma-server-threads))
	server
      (let* ((*kuma-request* request)
	     (*kuma-connection* connection)
	     (*kuma-server* (kuma-server listener))
	     (handler nil)
	     (handler-func nil)	     
	     (*kuma-response* (make-instance 'http-response)))
	(setf handler (loop for handler in handlers
			 for result = (handler-case 
					  (funcall (response-handler-condition handler))
					(error (e)
					  (print e)
					  nil))
			 when result
			 return handler))
	(if handler
	    (setf (status-line) +http-ok+
		  handler-func (lambda () (funcall (response-handler-function handler))))
	    (progn
	      (listener-disconnect listener *kuma-connection* fd socket :read)	      
	      (setf (status-line) +http-not-found+		    
		    handler-func (lambda () (funcall (response-handler-function error-handler))))))            
	(let ((callable (make-instance 'worker-callable
				       :fd fd
				       :socket socket
				       :server server
				       :listener listener
				       :handler-func handler-func
				       :response *kuma-response*
				       :request request
				       :connection connection)))
	  (setf (connection-callable connection) callable))))))

(defmethod kuma-server-process-response ((listener kuma-listener) (connection connection))
  (with-accessors ((callable-lock connection-callable-lock)
		   (worker-lock connection-worker-lock)
		   (callable connection-callable))
      connection

	  (let ((server-pool (kuma-server-threads (kuma-server listener)))
		(worker-lock-acquired-p nil))
	    (unwind-protect 
		 (progn
		   (setf worker-lock-acquired-p (bt:acquire-lock worker-lock))
		   (if worker-lock-acquired-p
		     (bt:with-lock-held (callable-lock)
		       (add-to-pool server-pool callable))
		     (arnesi:enqueue (connection-worker-pipeline connection) callable)))
	      (when worker-lock-acquired-p
		(bt:release-lock worker-lock))))))

(defgeneric kuma-server-run (server))
(defgeneric kuma-server-add-handler (server handler))
(defgeneric make-kuma-server-error-handler (server http-error))

(defclass kuma-server ()
  ((name :reader kuma-server-name :initarg :name)
   (thread-queue :reader kuma-server-thread-queue :initarg :thread-queue)
   (threads :accessor kuma-server-threads :initform nil)
   (disconnector-lock :reader disconnector-lock :initform (bt:make-lock))
   ;(conditions :accessor kuma-server-conditions :initform nil)
   ;(working-threads :accessor kuma-server-working-threads :initform nil)
   ;(queue-threads-lock :accessor kuma-server-queue-threads-lock :initform (bt:make-lock))
   (listeners :reader kuma-server-listeners :initarg :listeners)
   (idle-timeout :accessor kuma-server-idle-timeout :initarg :idle-timeout)
   (running-p :accessor kuma-server-running-p :initform nil)
   (handlers :accessor kuma-server-handlers :initarg :handlers)
   (error-handler :accessor kuma-server-error-handler :initarg :error-handler)
   (http-port :reader kuma-server-http-port :initarg :http-port)
   (http-bind-address :reader kuma-server-bind-address :initarg :bind-address))
  (:default-initargs :name "Kuma"
    :handlers nil
    :thread-queue 10
    :http-port 6080
    :idle-timeout *connection-idle-timeout*
    :bind-address +ipv4-unspecified+
    :error-handler (make-instance 'response-error-handler)))

(defmethod initialize-instance :after ((server kuma-server) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((queue-length kuma-server-thread-queue)
		   (threads kuma-server-threads)
		   (http-port kuma-server-http-port)
		   (bind-address kuma-server-bind-address))
      server
    (setf threads (make-thread-pool queue-length))
    (when (not (slot-boundp server 'listeners))
      (let ((http-local-socket (merge-pathnames 
				(make-pathname :name 
					       (format nil "local-socket-~a" 
						       (symbol-name (gensym))))
				*tmp-path*)))
	(setf (slot-value server 'listeners)
	      (list (make-instance 'http-listener 
				   :port http-port 
				   :bind-address bind-address 
				   :server server 
				   :local-socket http-local-socket)
		    #|
		    (make-instance 'http-response-listener 
				   :server server 
				   :local-socket http-local-socket)
		    |#
		    ))))))

(defmethod listener-disconnect ((listener kuma-listener) connection fd socket &rest events)
  (let ((lock (disconnector-lock (kuma-server listener))))
    (bt:with-lock-held (lock)
      (let ((event-base (kuma-event-base listener)))
	(if (not (intersection '(:read :write :error) events))
	    (remove-fd-handlers event-base fd :read t :write t :error t)
	    (if connection
		(progn
		  (when (and (member :read events) (io-handler-reader-p connection))
		    (remove-fd-handlers event-base fd :read t)
		    (setf (io-handler-reader-p connection) nil))
		  (when (and (member :write events) (io-handler-writer-p connection))
		    (remove-fd-handlers event-base fd :write t)
		    (setf (io-handler-writer-p connection) nil))
		  (when (member :error events)
		    (remove-fd-handlers event-base fd :error t)))
		(progn
		  (when (member :read events) 
		    (remove-fd-handlers event-base fd :read t))
		  (when (member :write events) 
		    (remove-fd-handlers event-base fd :write t))
		  (when (member :error events)
		    (remove-fd-handlers event-base fd :error t)))))
	;; and finally if were asked to close the socket, we do so here
	(when (member :close events)
	  (finish-output)
	  (close socket)
	  (remhash fd (kuma-open-connections listener)))))))


(defmethod  kuma-server-add-handler ((server kuma-server) handler)
  (push handler (kuma-server-handlers server)))

(defmethod kuma-server-run ((server kuma-server))
  (let ((*kuma-server* server))
    (with-accessors ((running-p kuma-server-running-p)
		     (listeners kuma-server-listeners)
		     (threads kuma-server-threads))
	*kuma-server*
      (setf running-p t)
      (start-pool threads)
      (loop for listener in listeners
	   collect (bt:make-thread (lambda () (kuma-listener-run listener)))))))





