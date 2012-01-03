;;; -*- lisp -*-
;;; $Header: conditions.lisp $

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

(define-condition http-error ()
  ((code :reader error-code :initarg :code)
   (description :reader error-description :initarg :description)
   (reason :reader error-reason :initarg :reason))
  (:report (lambda (condition stream)
	     (with-accessors ((code error-code)
			      (description error-description)
			      (reason error-reason))
		 condition
	       (format stream "~a:> ~a: ~a - ~a" 
		       (type-of condition)
		       code
		       description
		       (or reason "")))))
  (:default-initargs :reason nil))

(defun signal-http-error (error-code &optional reason)
  (signal 'http-error :code (first error-code) :description (second error-code) :reason reason))

(defun signal-basic-auth-required (realm)
  (let ((error-code +http-unauthorized+))
    (setf (response-param "WWW-Authenticate") (format nil "Basic realm=\"~a\"" realm))
    (signal 'http-error :code (first error-code) :description (second error-code))))