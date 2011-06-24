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


(defvar *max-bytes* 1)
(defconstant +read-timeout+ 10)
(defconstant +write-timeout+ 10)

(defvar *read-timeout* +read-timeout+)
(defvar *write-timeout* +write-timeout+)

(defvar *worker-function*)

(defparameter +http-header-separator+ (babel:string-to-octets
                                       (format nil "~a~a~a~a" #\Return #\Linefeed #\Return #\Linefeed)
                                       :encoding :ascii))

;;; ==================================================================================
(defvar *kuma-server* nil)

(defgeneric kuma-listener-run (listener))
(defgeneric kuma-listener-run-helper (listener))
(defgeneric make-kuma-listener-handler (listener socket))
(defgeneric make-kuma-listener-read-some-bytes (listener))
(defgeneric make-kuma-listener-write-some-bytes (listener))

(defgeneric make-kuma-listener-disconnector (listener socket))

(defgeneric kuma-listener-error-handler (listener fd http-error))

(defclass kuma-listener ()
  ((server :reader kuma-server :initarg :server)
   (open-connections :accessor kuma-open-connections :initform nil)
   (server-event-base :accessor kuma-event-base :initform nil)
   (port :accessor kuma-listener-port :initarg :port)
   (bind-address :accessor kuma-listener-bind-address :initarg :bind-address)
   (max-backlog :reader kuma-listener-max-backlog :initarg :max-backlog)
   (read-some-bytes :reader kuma-listener-read-some-bytes)
   (write-some-bytes :reader kuma-listener-write-some-bytes))
  (:default-initargs :bind-address +ipv4-unspecified+
    :max-backlog *default-backlog-size*))

(defmethod initialize-instance :after ((listener kuma-listener) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value listener 'read-some-bytes) (make-kuma-listener-read-some-bytes listener)
	(slot-value listener 'write-some-bytes) (make-kuma-listener-write-some-bytes listener)))

#|
(defmethod kuma-listener-error-handler ((listener kuma-listener) fd http-error)
  (let* ((connection (gethash fd (kuma-open-connections listener)))
         ;(who (connection-who connection))
         ;(port (connection-port connection))
         ;(socket (connection-client connection))
         (*kuma-request* (connection-request connection))
         (*kuma-response* (setf (connection-response connection) (make-instance 'http-response))))

    (setf (status-line) http-error)
    (let ((error-code (second (http-response-status-line *kuma-response*)))
          (reason (third (http-response-status-line *kuma-response*))))
      (setf (response-body)
            (format nil
                    "<html>
<head>
  <title>Error ~a</title>
  <style>body {
  font-family: arial, elvetica;
  font-size: 7pt;
}
span.blue {
  padding: 0 3px;
  background-color: #525D76;
  color: white;
  font-weight: bolder;
  margin-right: .25em;
}
p.h1, p.h2 {
  padding: 0 3px;
  background-color: #525D76;
  color: white;
  font-weight: bolder;
  font-size: 2em;
  margin: 0;
  margin-bottom: .5em;
}
p.h2 {font-size: 1.5em;}</style>
</head>
<body>
  <p>
    <p class='h1'>
      HTTP Status ~a - ~a
    </p>

    <hr noshade='noshade'>
    <p>
      <span class='blue'>url</span>
      ~a
    </p>
    <hr noshade='noshade'>
    <p class='h2'>
      ~a at ~a
    </p>
  </p>

</body>
</html>"
                    error-code
                    error-code
                    reason
                    (http-request-uri *kuma-request*)
                    (kuma-server-name *kuma-server*)
                    (get-request-param "Host"))))))
|#

(defmethod kuma-listener-run ((listener kuma-listener))
  (unwind-protect
       (handler-case
           (progn
             (setf (kuma-open-connections listener) (make-hash-table :test #'equalp)
                   (kuma-event-base listener) (make-instance 'event-base))
             (kuma-listener-run-helper listener))
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

(defmethod kuma-listener-run-helper ((listener kuma-listener))
  (let ((bind-address (kuma-listener-bind-address listener))
        (port (kuma-listener-port listener))
        (max-backlog (kuma-listener-max-backlog listener))
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
                            (make-kuma-listener-handler listener server))
           (handler-case
               (event-dispatch (kuma-event-base listener))
             (socket-connection-reset-error ()
               (format t "Unexpected connection reset by peer!~%"))
             (hangup () (format t "Unexpected hangup!~%"))
             (end-of-file () (format t "Unexpeected end of file!~%"))))
      (close server))))

(defmethod make-kuma-listener-handler ((listener kuma-listener) socket)
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
                          (kuma-listener-read-some-bytes listener)))))))

(defmethod make-kuma-listener-write-some-bytes ((listener kuma-listener))
  (lambda (fd event exception)
    (declare (ignore event exception))
    (let* ((*kuma-connection* (gethash fd (kuma-open-connections listener)))
	   (*kuma-server* (kuma-server listener)))
      (with-accessors ((who connection-who)
		       (port connection-port)
		       (socket connection-client)
		       (request connection-request)
		       (write-buffer connection-write-buffer))
	  *kuma-connection*
	(if write-buffer
	    (handler-case
		(let ((ch (read-byte write-buffer)))
		  (handler-case
		      (send-to socket
			       (make-array 1 :element-type 'unsigned-byte :initial-element ch)
			       :start 0
			       :end 1)
		    (socket-connection-reset-error ()
		      ;; If for somer eaon the client reset the network connection,
		      ;; we'll get this signal.
		      (funcall (make-kuma-listener-disconnector listener socket) who port :close))
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
		      (funcall (make-kuma-listener-disconnector listener socket) who port :close))
		    (end-of-file ()
		      (funcall (make-kuma-listener-disconnector listener socket) who port :close))))
	      (end-of-file ()
		(with-accessors  ((http-response-reader connection-response)
				  (worker-lock connection-worker-lock)
				  (pipeline connection-request-pipeline)
				  (connection-write-buffer connection-write-buffer))
		    *kuma-connection*
		  (close http-response-reader)
		  (bt:with-lock-held (worker-lock)
		     (setf http-response-reader (arnesi:dequeue pipeline)))
		  (if http-response-reader
		      (progn
			(setf connection-write-buffer (create-response-stream *kuma-connection*))
			(funcall (kuma-listener-write-some-bytes listener) fd :write nil))
		      (funcall (make-kuma-listener-disconnector listener socket) who port :write)))))
	    (funcall (make-kuma-listener-disconnector listener socket) who port :write))))))

(defmethod make-kuma-listener-read-some-bytes ((listener kuma-listener))
  (lambda (fd event exception)
    (declare (ignore event exception))
    (let* ((*kuma-server* (kuma-server listener))
	   (connection (gethash fd (kuma-open-connections listener)))
           (who (connection-who connection))
           (port (connection-port connection))
           (socket (connection-client connection))
           (read-buf (make-array *max-bytes* :element-type 'unsigned-byte)))
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
            (unless (zerop bytes-read)
                (progn
                  (setf (connection-read-buffer connection)
                        (concatenate '(vector (unsigned-byte 8))
                                     (connection-read-buffer connection)
                                     (subseq read-buf 0 bytes-read)))
                  (let ((current-buffer (connection-read-buffer connection)))
                    (if (not (header-headers (http-request-header (connection-request connection))))
                        (when (and (> (length current-buffer) 4)
                                 (equalp (subseq current-buffer (- (length current-buffer) 4))
                                         +http-header-separator+))
                            ;;parse header
			  (let* ((request (connection-request connection))
				 (request-header (http-request-header request))
				 (expect (header-expect request-header)))
			    (setf (slot-value (http-request-header (connection-request connection)) 'headers)
				  (%parse-headers current-buffer)
				  (connection-read-buffer connection) nil)
			    (when expect
			      (if (string-equal expect "100-continue")
				  (let ((continue
					 (babel:string-to-octets (make-http-line
								  (make-http-line
								   (format nil "HTTP/1.1 ~{~a~^ ~}" +http-continue+)))
								 :encoding :ascii)))
				    (send-to socket
					     continue
					     :end (length continue))
				    (setf (header-expect request-header) nil))))
			    (format t "Adding handler ~a~%" *kuma-server*)
			    (with-accessors ((handlers kuma-server-handlers))
				*kuma-server*
			      (let ((worker (loop for handler in handlers
					       for result = (funcall handler)
					       when result
					       return result)))
				(unless worker (setf worker (lambda () #P"/home/kiuma/pippo.js"))) ;;remove me
				(if worker
				    (thread-pool:add-to-pool (kuma-server-threads *kuma-server*)
							     (lambda ()
								     (let* ((*kuma-connection* connection)
									    (*kuma-request* request)
									    (response-reader)
									    (*kuma-response* (make-instance 'http-response))
									    (worker-lock (connection-worker-lock connection)))

								       (setf (http-response-body-content *kuma-response*)
									     (funcall worker)
									     response-reader (make-instance 'http-response-reader))
								       (format t "Calling worker~%")
								       (bt:with-lock-held (worker-lock)
									 (arnesi:enqueue (connection-request-pipeline *kuma-connection*)
											 response-reader))
								       (unless (connection-response *kuma-connection*)
									 (unless (connection-write-buffer *kuma-connection*)
									   (setf (connection-response *kuma-connection*) 
										 (arnesi:dequeue (connection-request-pipeline *kuma-connection*)) 
										 (connection-write-buffer *kuma-connection*)
										 (create-response-stream response-reader))
									   (set-io-handler (kuma-event-base listener)
											   fd
											   :write
											   (kuma-listener-write-some-bytes listener)))
									 (funcall (kuma-listener-write-some-bytes listener) fd :write nil)))
									  )))))
			  )
                        ;; parsebody ?
                        ))))))

        (socket-connection-reset-error ()
          ;; Handle the client sending a reset.
          (let* ()
            (format t "Client ~A:~A: connection reset by peer.~%" who port)
            (funcall (make-kuma-listener-diconnector listener socket) who port :close)))
        (end-of-file ()

          (format t "read eof~%")
          (funcall (make-kuma-listener-diconnector listener socket) who port :close)
          #|
          (setf (connection-write-buffer connection)
          (babel:string-to-octets
          (format nil "~%Request:~%~a~%Reply:~%~a~%END~%"
          (babel:octets-to-string (connection-read-buffer connection))
          *test-string*)))
          (write-some-bytes fd :write nil)
          |#
          )))))

(defmethod make-kuma-listener-disconnector ((listener kuma-listener) socket)
  (lambda (who port &rest events)
    (let ((fd (socket-os-fd socket))
	  (event-base (kuma-event-base listener)))
      (format t "(make-kuma-listener-disconnector ~a ~a) => event-base: ~a fd: ~a~%" listener socket event-base fd)
      (if (not (intersection '(:read :write :error) events))
          (remove-fd-handlers event-base fd :read t :write t :error t)
          (progn
            (when (member :read events)
              (remove-fd-handlers event-base fd :read t))
            (when (member :write events)
              (remove-fd-handlers event-base fd :write t))
            (when (member :error events)
              (remove-fd-handlers event-base fd :error t))))
      ;; and finally if were asked to close the socket, we do so here
      (when (member :close events)
        (format t "Closing connection to ~A:~A~%" who port)
        (finish-output)
        (close socket)
        (remhash fd (kuma-open-connections listener))))))



(defgeneric kuma-server-run (server))
(defgeneric kuma-server-add-handler (server handler))
(defgeneric make-kuma-server-error-handler (server http-error))

(defclass kuma-server ()
  ((name :reader kuma-server-name :initarg :name)
   (thread-queue :reader kuma-server-thread-queue :initarg :thread-queue)
   (threads :accessor kuma-server-threads :initform nil)
   ;(conditions :accessor kuma-server-conditions :initform nil)
   ;(working-threads :accessor kuma-server-working-threads :initform nil)
   ;(queue-threads-lock :accessor kuma-server-queue-threads-lock :initform (bt:make-lock))
   (listeners :reader kuma-server-listeners :initarg :listeners)

   (running-p :accessor kuma-server-running-p :initform nil)
   (handlers :accessor kuma-server-handlers :initarg :handlers)
   (http-port :reader kuma-server-http-port :initarg :http-port)
   (http-bind-address :reader kuma-server-bind-address :initarg :bind-address))
  (:default-initargs :name "Kuma" :handlers nil :thread-queue 10 :http-port 6080 :bind-address +ipv4-unspecified+))

(defmethod initialize-instance :after ((server kuma-server) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((queue-length kuma-server-thread-queue)
		   (threads kuma-server-threads)
		   (http-port kuma-server-http-port)
		   (bind-address kuma-server-bind-address))
      server
    (setf threads (thread-pool:make-thread-pool queue-length))
    (when (not (slot-boundp server 'listeners))
      (setf (slot-value server 'listeners)
	    (list (make-instance 'kuma-listener :port http-port :bind-address bind-address :server server))))))

(defmethod  kuma-server-add-handler ((server kuma-server) handler)
  (push handler (kuma-server-handlers server)))

(defmethod kuma-server-run ((server kuma-server))
  (let ((*kuma-server* server))
    (with-accessors ((running-p kuma-server-running-p)
		     (listeners kuma-server-listeners)
		     (threads kuma-server-threads))
	*kuma-server*
      (setf running-p t)
      (thread-pool:start-pool threads)
      (loop for listener in listeners
	   collect (bt:make-thread (lambda () (kuma-listener-run listener)))))))



#|--------------------------------------------------|#

#|
(defmethod do-connection-output ((server kuma-server) (connection connection))
  (let* ((request (connection-request connection))
         (response (connection-response connection))
         (keep-alive-p (string-equal (get-request-param "connection" request) "keep-alive"))
         (client-http-version (http-request-http-version request))
         (response-status-code (second (http-response-status-line response))))
    (seft (connection-write-buffer connection)
          (make-concatenated-stream
           (babel:string-to-octets (format nil "~a~a~a" (status-line response) #\Return #\Newline)
                                   :encoding :ascii)))))
|#