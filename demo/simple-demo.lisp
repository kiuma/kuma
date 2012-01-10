;;; -*- lisp -*-
;;; $Header: simple-demo.lisp $

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


(in-package :cl-user)

(use-package :kuma)

(defvar *demo-file* (load-time-value
		     (or #.*compile-file-pathname* *load-pathname*)))

(defvar *home-hanlder* (make-instance 'kuma:response-handler 
					     :condition-lambda (lambda ()
								 (or (string-equal "/" (kuma::http-request-location kuma:*kuma-request*))
								     (string-equal "/index.html" (kuma::http-request-location kuma:*kuma-request*))))
					     :handler-function (lambda () "<html>
  <head>
    <title>Kuma home</title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
  </head>
  <body>
    <h1>Merry Christmas 2011</h1>
    Kuma http server
  </body>
</html>
")))

(defvar *empty-handler* (make-instance 'kuma:response-handler 
				       :condition-lambda (lambda ()
							   (string-equal "/empty.html" (kuma::http-request-location kuma:*kuma-request*)))
				       :handler-function (lambda () nil)))

(defvar *file-handler* (make-instance 'kuma:response-handler
				      :condition-lambda (lambda ()
							   (string-equal "/file.html" (kuma::http-request-location kuma:*kuma-request*)))
				      :handler-function (lambda ()
							  *demo-file*)))


(defvar *idea-handler* (make-instance 'kuma:response-handler
				      :condition-lambda (lambda ()
							   (string-equal "/idea.html" (kuma::http-request-location kuma:*kuma-request*)))
				      :handler-function (lambda ()
							  #P"/home/kiuma/dld/java/idea/ideaIU-11.tar.gz")))

(defvar *protected-handler* (make-instance 'kuma:response-handler
				      :condition-lambda (lambda ()
							   (string-equal "/protected.html" (kuma::http-request-location kuma:*kuma-request*)))
				      :handler-function (lambda ()
							  (kuma::signal-basic-auth-required "kiuma"))))

(defvar *server* (make-instance 'kuma:kuma-server :handlers (list *home-hanlder* 
								  *empty-handler* 
								  *file-handler*
								  *protected-handler*
								  *idea-handler*)))

;;(kuma:kuma-server-run *server*)