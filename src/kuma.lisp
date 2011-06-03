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



(defvar *server-event-base*)

(defvar *server-open-connections*)


(defvar *max-bytes* 1)
(defconstant +read-timeout+ 10)
(defconstant +write-timeout+ 10)

(defvar *read-timeout* +read-timeout+)
(defvar *write-timeout* +write-timeout+)

(defvar *worker-function*)

(defparameter +http-header-separator+ (babel:string-to-octets 
                                       (format nil "~a~a~a~a" #\Return #\Linefeed #\Return #\Linefeed) 
                                       :encoding :ascii))

(defgeneric do-connection-output (server connection))

(defgeneric write-some-bytes (fd event exception))
(defgeneric read-some-bytes (fd event exception))
(defgeneric parse-body (fd))



(defmethod write-some-bytes (fd event exception)
  (declare (ignore event exception))
  (let* ((connection (gethash fd *server-open-connections*))
         (who (connection-who connection))
         (port (connection-port connection))
         (socket (connection-client connection))
         (output-buffer (connection-write-buffer connection)))
    (when (and output-buffer (not (zerop (length output-buffer))))
      (format t "INFO output buffer is not null and length is ~a~%" (length output-buffer))
        (handler-case
            (let* ((buffer-length (length output-buffer))
                   (size (min buffer-length *max-bytes*)))
              (if (zerop size)
                  (error 'end-of-file)
                  (progn
                    (send-to socket output-buffer
                             :start 0
                             :end size)
                    (setf (connection-write-buffer connection) (subseq output-buffer size))
                    (format t "setting io-handler for event WRITE (2)~%"))))
          (socket-connection-reset-error ()
            ;; If for somer eaon the client reset the network connection,
            ;; we'll get this signal.
            (format t "Client ~A:~A: connection reset by peer.~%" who port)
            (funcall (make-server-disconnector socket) who port :close))

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
            (format t "Client ~A:~A got hangup on write.~%" who port)
            (funcall (make-server-disconnector socket) who port :close))
          (end-of-file () 
            (funcall (make-server-disconnector socket) who port :write)
            (unless (http-request-done-p (connection-request connection))
              (set-io-handler *server-event-base*
                              fd
                              :read
                              #'read-some-bytes)))))))

(defmethod read-some-bytes (fd event exception)
  (declare (ignore event exception))
  (let* ((connection (gethash fd *server-open-connections*))
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
        (if (zerop bytes-read)
            (progn 
              (error 'end-of-file))
            (progn              
              (setf (connection-read-buffer connection)
                    (concatenate '(vector (unsigned-byte 8)) 
                                 (connection-read-buffer connection)
                                 (subseq read-buf 0 bytes-read))) 
              (let ((current-buffer (connection-read-buffer connection)))
                (if (not (header-headers (http-request-header (connection-request connection))))
                    (if (and (> (length current-buffer) 4)
                             (equalp (subseq current-buffer (- (length current-buffer) 4))
                                     +http-header-separator+))
                        ;;parse header
                        (progn
                          (format t "=*=======================*=~%~a=*===========================*=~%" 
                                    (babel:octets-to-string (connection-read-buffer connection) :encoding :ascii))
                          (let ((header-lines (cl-ppcre:split "\\r\\n" 
                                                              (babel:octets-to-string (connection-read-buffer connection) 
                                                                                      :encoding :ascii))))
                            (setf (slot-value (http-request-header (connection-request connection)) 'request-line)
                                  (%parse-http-message header-lines)
                                  (slot-value (http-request-header (connection-request connection)) 'headers)
                                  (%parse-headers header-lines)
                                  (connection-read-buffer connection) nil))

                          (let* ((request (connection-request connection))
                                 (request-header (http-request-header request))                                 
                                 (expect (header-expect request-header)))
                            (when expect
                              (if (string-equal expect "100-continue")
                                  (progn (funcall (make-server-disconnector socket) who port :read)
                                         )))
                            (format t "header:~%~a~%=*============*=~%" 
                                    (slot-value request-header 'headers))) ; remove me
                          (error 'socket-connection-reset-error)
                          ))
                    ;; parsebody ?
                    )))))         

      (socket-connection-reset-error ()
        ;; Handle the client sending a reset.
        (let* ()
          (format t "Client ~A:~A: connection reset by peer.~%" who port)
          (funcall (make-server-disconnector socket) who port :close)))
      (end-of-file () 
        
        (format t "read eof~%")
        (funcall (make-server-disconnector socket) who port :close)
        #|
        (setf (connection-write-buffer connection)
              (babel:string-to-octets 
               (format nil "~%Request:~%~a~%Reply:~%~a~%END~%"
                       (babel:octets-to-string (connection-read-buffer connection))
                       *test-string*)))
        (write-some-bytes fd :write nil)
        |#
        ))))




(defun run-server-helper (port &key 
                          ipv6-p 
                          (bind-address +loopback+) 
                          (external-format '(:utf-8 :eol-style :crlf)) 
                          (max-backlog *default-backlog-size*))
  (let ((server (make-socket :connect :passive
                             :address-family :internet
                             :type :stream
                             :ipv6 ipv6-p
                             :external-format external-format)))
    (unwind-protect 
         (progn
           (bind-address server bind-address :port port :reuse-address t)
           (listen-on server :backlog max-backlog)
           (set-io-handler  *server-event-base*
                           (socket-os-fd server)
                           :read
                           (make-server-listener-handler server))
           (handler-case
               (event-dispatch *server-event-base*)
             (socket-connection-reset-error ()
               (format t "Unexpected connection reset by peer!~%"))
             (hangup () (format t "Unexpected hangup!~%"))
             (end-of-file () (format t "Unexpeected end of file!~%"))))
      (close server))))


(defun make-server-listener-handler (socket)
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
          (setf (gethash client-fd *server-open-connections*) (make-instance 'connection 
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
          (set-io-handler *server-event-base*
                          client-fd
                          :read
                          #'read-some-bytes)
          (format t "setting io-handler for event WRITE (1)~%")
          (format t "Monitoring FD ~a for event WRITE~%" client-fd)
          (set-io-handler *server-event-base*
                          client-fd
                          :write
                          #'write-some-bytes))))))

(defun make-server-disconnector (socket)
  ;; When this function is called, it can be told which callback to remove, if
  ;; no callbacks are specified, all of them are removed! The socket can be
  ;; additionally told to be closed.
  (lambda (who port &rest events)
    (let ((fd (socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
          (remove-fd-handlers *server-event-base* fd :read t :write t :error t)
          (progn
            (when (member :read events)
              (remove-fd-handlers *server-event-base* fd :read t))
            (when (member :write events)
              (remove-fd-handlers *server-event-base* fd :write t))
            (when (member :error events)
              (remove-fd-handlers *server-event-base* fd :error t))))
      ;; and finally if were asked to close the socket, we do so here
      (when (member :close events)
        (format t "Closing connection to ~A:~A~%" who port)
        (finish-output)
        (close socket)
        (remhash fd *server-open-connections*)))))

(defun run-server (&key (port 9999) (bind-address iolib.sockets:+ipv4-unspecified+))
  (let ((*server-open-connections* nil)
        (*server-event-base* nil))
    (unwind-protect
         (handler-case
             (progn
               (setf *server-open-connections* (make-hash-table :test #'equalp)
                     *server-event-base* (make-instance 'event-base))

               (run-server-helper port :bind-address bind-address))

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
       *server-open-connections*)

      ;; and clean up the event-base too!
      (when *server-event-base*
        (close *server-event-base*))
      (format t "Server Exited.~%")
      (finish-output))))

(defvar *kuma-server* nil)

(defgeneric kuma-listener-run (listener)) 
(defgeneric kuma-listener-run-helper (listener))
(defgeneric make-kuma-listener-handler (listener socket))
(defgeneric make-kuma-listener-read-some-bytes (listener))
(defgeneric make-kuma-listener-write-some-bytes (listener))

(defgeneric make-kuma-listener-diconnector (listener socket))

(defgeneric kuma-listener-error-handler (listener fd http-error))

(defclass kuma-listener ()
  ((server :reader kuma-server :initarg :server)
   (open-connections :accessor kuma-open-connections :initform nil)
   (server-event-base :accessor kuma-event-base :initform nil)
   (port :accessor kuma-listener-port :initarg :port)
   (bind-address :accessor kuma-listener-bind-address :initarg :bind-address)
   (max-backlog :reader kuma-listener-max-backlog :initarg :max-backlog))
  (:default-initargs :bind-address +ipv4-unspecified+ 
    :max-backlog *default-backlog-size*))

(defmethod kuma-listener-error-handler ((listener kuma-listener) fd http-error)
  (let* ((connection (gethash fd *server-open-connections*))
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
                          (make-kuma-listener-read-some-bytes listener))
          (format t "setting io-handler for event WRITE (1)~%")
          (format t "Monitoring FD ~a for event WRITE~%" client-fd)
          (set-io-handler (kuma-event-base listener)
                          client-fd
                          :write
                          #'write-some-bytes))))))

(defmethod make-kuma-listener-write-some-bytes ((listener kuma-listener))
  (lambda (fd event exception)
    (declare (ignore event exception))
    (let* ((connection (gethash fd *server-open-connections*))
           (who (connection-who connection))
           (port (connection-port connection))
           (socket (connection-client connection))
           (continue-stream (connection-continue-stream connection)))
      (if continue-stream
          (handler-case
              (let ((ch (read-byte continue-stream)))
                (handler-case
                    (send-to socket
                             (make-array 1 :element-type 'unsigned-byte :initial-element ch)
                             :start 0
                             :end 1)
                  (socket-connection-reset-error ()
                    ;; If for somer eaon the client reset the network connection,
                    ;; we'll get this signal.
                    (funcall (make-server-disconnector socket) who port :close))
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
                    (funcall (make-server-disconnector socket) who port :close))
                  (end-of-file ()
                    (funcall (make-server-disconnector socket) who port :close))))
            (end-of-file ()
              (funcall (make-kuma-listener-diconnector listener socket) who port :write)
              (close continue-stream)
              (setf (connection-continue-stream connection) nil
                    (header-expect (http-request-header (connection-request connection))) nil)
              (set-io-handler (kuma-event-base listener)
                          fd
                          :read
                          (make-kuma-listener-read-some-bytes listener))))))))

(defmethod make-kuma-listener-read-some-bytes ((listener kuma-listener)) 
  (lambda (fd event exception)
    (declare (ignore event exception))
    (let* ((connection (gethash fd *server-open-connections*))
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
            (if (zerop bytes-read)
                (progn 
                  (error 'end-of-file))
                (progn              
                  (setf (connection-read-buffer connection)
                        (concatenate '(vector (unsigned-byte 8)) 
                                     (connection-read-buffer connection)
                                     (subseq read-buf 0 bytes-read))) 
                  (let ((current-buffer (connection-read-buffer connection)))
                    (if (not (header-headers (http-request-header (connection-request connection))))
                        (if (and (> (length current-buffer) 4)
                                 (equalp (subseq current-buffer (- (length current-buffer) 4))
                                         +http-header-separator+))
                            ;;parse header
                            (progn
                              (setf (slot-value (http-request-header (connection-request connection)) 'headers)
                                    (%parse-headers current-buffer)
                                    (connection-read-buffer connection) nil)

                              (let* ((request (connection-request connection))
                                     (request-header (http-request-header request))                                 
                                     (expect (header-expect request-header)))
                                (when expect
                                  (funcall (make-kuma-listener-diconnector listener socket) who port :read)
                                  (if (string-equal expect "100-continue")
                                      (progn (setf (connection-continue-stream connection)
                                                   (babel:string-to-octets (format nil "HTTP/1.1 ~{~a~^ ~}" +http-continue+)
                                                                           :encoding :ascii))
                                             (set-io-handler (kuma-event-base listener)
                                                             fd
                                                             :write
                                                             (make-kuma-listener-write-some-bytes listener))
                                             (funcall (make-kuma-listener-write-some-bytes listener) fd :write nil))))
                                (format t "header:~%~a~%=*============*=~%" 
                                        (slot-value request-header 'headers))) ; remove me
                              (error 'socket-connection-reset-error)
                              ))
                        ;; parsebody ?
                        )))))         

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

(defmethod make-kuma-listener-diconnector ((listener kuma-listener) socket)
  (lambda (who port &rest events)
    (let ((fd (socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
          (remove-fd-handlers *server-event-base* fd :read t :write t :error t)
          (progn
            (when (member :read events)
              (remove-fd-handlers *server-event-base* fd :read t))
            (when (member :write events)
              (remove-fd-handlers *server-event-base* fd :write t))
            (when (member :error events)
              (remove-fd-handlers *server-event-base* fd :error t))))
      ;; and finally if were asked to close the socket, we do so here
      (when (member :close events)
        (format t "Closing connection to ~A:~A~%" who port)
        (finish-output)
        (close socket)
        (remhash fd *server-open-connections*)))))



(defgeneric kuma-server-run (server))
(defgeneric kuma-server-worker (server index))
(defgeneric kuma-server-add-handler (server handler))
(defgeneric make-kuma-server-error-handler (server http-error))

(defclass kuma-server ()
  ((name :reader kuma-server-name :initarg :name)
   (thread-queue :reader kuma-server-thread-queue :initarg :thread-queue)
   (threads :accessor kuma-server-threads :initform nil)
   (conditions :accessor kuma-server-conditions :initform nil)
   (working-threads :accessor kuma-server-working-threads :initform nil)
   (queue-threads-lock :accessor kuma-server-queue-threads-lock :initform (bt:make-lock))
   (listeners :reader kuma-server-listeners :initarg :listeners)
   (running-p :accessor kuma-server-running-p :initform nil)
   (handlers :accessor kuma-server-handlers :initarg :handlers))
  (:default-initargs :name "Kuma" :handlers nil))

(defmethod  kuma-server-add-handler ((server kuma-server) handler)
  (push handler (kuma-server-handlers server)))

(defmethod kuma-server-run ((server kuma-server))
  (let ((*kuma-server* (kuma-server-name server)))
    (setf (kuma-server-running-p server) 
          t
          (kuma-server-conditions server) 
          (loop for i upto (- (length (kuma-server-thread-queue server)) 1)
             collect (bt:make-condition-variable)
             collect t)
          (kuma-server-threads server) 
          (loop for tn upto (- (length (kuma-server-thread-queue server)) 1)
             for lock = (bt:make-lock)
             collect (bt:make-thread (lambda () 
                                       (bt:with-lock-held (lock)
                                         (loop while (kuma-server-running-p server)
                                            do (let ((condition (nth (* tn 2) (kuma-server-conditions server)))
                                                     (queue-threads-lock (kuma-server-queue-threads-lock server)))
                                                 (bt:condition-wait (nth (* tn 2) 
                                                                         (kuma-server-conditions server)) 
                                                                    lock)
                                                 (when (kuma-server-running-p server)
                                                   (bt:with-lock-held (queue-threads-lock)
                                                     (setf (getf (kuma-server-conditions server) condition) nil))
                                                   (kuma-server-worker server tn)
                                                   (bt:with-lock-held (queue-threads-lock)
                                                     (setf (getf (kuma-server-conditions server) condition) t))))))))))))

(defmethod kuma-server-worker ((server kuma-server) index)
  )

#|--------------------------------------------------|#


(defmethod do-connection-output ((server kuma-server) (connection connection))
  (let* ((request (connection-request connection))
         (response (connection-response connection))
         (keep-alive-p (string-equal (get-request-param "connection" request) "keep-alive"))
         (client-http-version (http-request-http-version request))
         (response-status-code (second (http-response-status-line response))))
    (seft (connection-write-buffer connection)
          (make-concatenated-stream
           (babel:string-to-octets (fromat nil "~a~a~a" (status-line response) #\Return #\Newline)
                                   :encoding :ascii)))))